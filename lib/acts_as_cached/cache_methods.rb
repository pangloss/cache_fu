module ActsAsCached
  module ClassMethods
    @@nil_sentinel = :_nil

    def cache_config
      config = ActsAsCached::Config.class_config[cache_name] ||= {}
      if name == cache_name
        config
      else
        # sti
        ActsAsCached::Config.class_config[name] ||= config.dup
      end
    end

    def cache_options
      cache_config[:options] ||= {}
    end

    def get_cache(*args)
      options = args.last.is_a?(Hash) ? args.pop : {}
      args    = args.flatten

      ##
      # head off to get_caches if we were passed multiple cache_ids
      if args.size > 1 
        return get_caches(args, options) 
      else
        cache_id = args.first
      end

      if (item = fetch_cache(cache_id)).nil?
        set_cache(cache_id, block_given? ? yield : fetch_cachable_data(cache_id), options[:ttl])
      else
        @@nil_sentinel == item ? nil : item
      end
    end

    ##
    # This method accepts an array of cache_ids which it will use to call 
    # get_multi on your cache store.  Any misses will be fetched and saved to 
    # the cache, and a hash keyed by cache_id will ultimately be returned.
    #
    # If your cache store does not support #get_multi an exception will be raised.
    def get_caches(*args)
      raise NoGetMulti unless cache_store.respond_to? :get_multi

      options   = args.last.is_a?(Hash) ? args.pop : {}
      cache_ids = args.flatten.map(&:to_s)
      keys      = cache_keys(cache_ids)

      # Map memcache keys to object cache_ids in { memcache_key => object_id } format
      keys_map = Hash[*keys.zip(cache_ids).flatten]

      # Call get_multi and figure out which keys were missed based on what was a hit
      hits = cache_store(:get_multi, *keys) || {}

      # Misses can take the form of key => nil
      hits.delete_if { |key, value| value.nil? }

      misses = keys - hits.keys
      hits.each { |k, v| hits[k] = nil if v == @@nil_sentinel }

      # Return our hash if there are no misses
      return hits.values.index_by(&:cache_id) if misses.empty?

      # Find any missed records
      needed_ids     = keys_map.values_at(*misses)
      missed_records = Array(fetch_cachable_data(needed_ids))

      # Cache the missed records
      missed_records.each { |missed_record| missed_record.set_cache(options[:ttl]) }

      # Return all records as a hash indexed by object cache_id
      (hits.values + missed_records).index_by(&:cache_id)
    end

    def set_cache(cache_id, value, ttl = nil)
      returning(value) do |v|
        #logger.fatal { "set #{ cache_id } with #{ Marshal.dump(value).length / 1024 }K" }
        v = @@nil_sentinel if v.nil?
        cache_store(:set, cache_key(cache_id), v, ttl || cache_config[:ttl] || 1500)
      end
    end

    def expire_cache(cache_id = nil)
      cache_store(:delete, cache_key(cache_id)) 
      true
    end
    alias :clear_cache :expire_cache

    def reset_cache(cache_id = nil)
      set_cache(cache_id, fetch_cachable_data(cache_id))
    end

    ##
    # Encapsulates the pattern of writing custom cache methods
    # which do nothing but wrap custom finders.
    #
    #   => Story.caches(:find_popular)
    #
    #   is the same as
    #
    #   def self.cached_find_popular
    #     get_cache(:find_popular) { find_popular }
    #   end
    #
    #  The method also accepts both a :ttl and/or a :with key.
    #  Obviously the :ttl value controls how long this method will
    #  stay cached, while the :with key's value will be passed along
    #  to the method.  The hash of the :with key will be stored with the key,
    #  making two near-identical #caches calls with different :with values utilize
    #  different caches.
    #
    #  => Story.caches(:find_popular, :with => :today)
    #
    #  is the same as
    #
    #   def self.cached_find_popular
    #     get_cache("find_popular:today") { find_popular(:today) }
    #   end
    #   
    # If your target method accepts multiple parameters, pass :withs an array.
    #
    # => Story.caches(:find_popular, :withs => [ :one, :two ])
    #
    # is the same as
    #
    #   def self.cached_find_popular
    #     get_cache("find_popular:onetwo") { find_popular(:one, :two) }
    #   end
    def caches(method, options = {})
      if options.keys.include?(:with) 
        with = options.delete(:with)
        get_cache("#{method}:#{with}", options) { send(method, with) }
      elsif withs = options.delete(:withs)
        get_cache("#{method}:#{withs}", options) { send(method, *withs) }
      else
        get_cache(method, options) { send(method) }
      end
    end
    alias :cached :caches

    def cached?(cache_id = nil)
      fetch_cache(cache_id).nil? ? false : true
    end
    alias :is_cached? :cached?

    def fetch_cache(cache_id)
      return if ActsAsCached.config[:skip_gets]

      value = autoload_missing_constants do 
        cache_store(:get, cache_key(cache_id))
      end
      #logger.fatal { "fetch #{ cache_id } with #{ (Marshal.dump(value).length / 1024.0 * 100).round / 100 }K" }
      value
    end

    def fetch_cachable_data(cache_id = nil)
      finder = cache_config[:finder] || :find
      return send(finder) unless cache_id

      args = [cache_id]
      args << cache_options.dup unless cache_options.blank?
      send(finder, *args)
    end
    
    def cache_namespace
      cache_store(:namespace)
    end
    
    # Memcache-client automatically prepends the namespace, plus a colon, onto keys, so we take that into account for the max key length.
    # Rob Sanheim
    def max_key_length
      key_size = cache_config[:key_size] || 250
      @max_key_length ||= cache_namespace ? (key_size - cache_namespace.length - 1) : key_size
    end

    def cache_name
      @cache_name ||= respond_to?(:base_class) ? base_class.name : name
    end

    def cache_keys(*cache_ids)
      cache_ids.flatten.map { |cache_id| cache_key(cache_id) }
    end

    def cache_key(cache_id)
      [cache_name, cache_config[:version], cache_id].compact.join(':').gsub(' ', '_').first(max_key_length)
    end

    def cache_store(method = nil, *args)
      return cache_config[:store] unless method

      load_constants = %w( get get_multi ).include? method.to_s

      swallow_or_raise_cache_errors(load_constants) do
        cache_config[:store].send(method, *args)
      end
    end

    def swallow_or_raise_cache_errors(load_constants = false, &block)
      load_constants ? autoload_missing_constants(&block) : yield
    rescue NoMethodError, ArgumentError, MemCache::MemCacheError => error
      if ActsAsCached.config[:raise_errors]
        raise error
      else
        RAILS_DEFAULT_LOGGER.debug "MemCache Error: #{error.message}" rescue nil
        nil
      end
    rescue TypeError => error
      if error.to_s.include? 'Proc' 
        raise MarshalError, "Most likely an association callback defined with a Proc is triggered, see http://ar.rubyonrails.com/classes/ActiveRecord/Associations/ClassMethods.html (Association Callbacks) for details on converting this to a method based callback" 
      else
        raise error
      end
    end

    def autoload_missing_constants
      yield
    rescue ArgumentError, MemCache::MemCacheError => error
      lazy_load ||= Hash.new { |hash, hash_key| hash[hash_key] = true; false }
      if error.to_s[/undefined class|referred/] && !lazy_load[error.to_s.split.last.constantize] then retry
      else raise error end
    end

    def add_expire_key_to(target, key, extra_exp_rules = [])
      target.add_scoped_expire_key cache_key(key), extra_exp_rules
    end

  end

  module InstanceMethods
    def self.included(base)
      base.send :delegate, :cache_config,  :to => 'self.class'
      base.send :delegate, :cache_options, :to => 'self.class'
    end

    def get_cache(key = nil, options = {}, &block)
      self.class.get_cache(cache_id(key), options, &block)
    end

    def set_cache(ttl = nil)
      self.class.set_cache(cache_id, self, ttl)
    end

    def reset_cache(key = nil)
      self.class.reset_cache(cache_id(key))
    end

    def expire_cache(key = nil)
      self.class.expire_cache(cache_id(key))
    end
    alias :clear_cache :expire_cache

    def cached?(key = nil)
      self.class.cached? cache_id(key)
    end

    def cache_key
      self.class.cache_key(cache_id)
    end
    
    def cache_id(key = nil)
      id = send(cache_config[:cache_id] || :id)
      key.nil? ? id : "#{id}:#{key}"
    end

    def caches(method, options = {})
      key = "#{id}:#{method}"
      if options.keys.include?(:with)
        with = options.delete(:with)
        self.class.get_cache("#{key}:#{with}", options) { send(method, with) }
      elsif withs = options.delete(:withs)
        self.class.get_cache("#{key}:#{withs}", options) { send(method, *withs) }
      else
        self.class.get_cache(key, options) { send(method) }
      end
    end
    alias :cached :caches

    # Ryan King
    def set_cache_with_associations
      Array(cache_options[:include]).each do |assoc|
        send(assoc).reload
      end if cache_options[:include]
      set_cache
    end

    # Lourens Naudé
    def expire_cache_with_associations(*associations_to_sweep)
      (Array(cache_options[:include]) + associations_to_sweep).flatten.uniq.compact.each do |assoc|
        Array(send(assoc)).compact.each { |item| item.expire_cache if item.respond_to?(:expire_cache) }
      end 
      expire_cache
    end

    def add_expire_key_to(target, key, extra_exp_rules = [])
      target.add_scoped_expire_key self.class.cache_key("#{ id }:#{ key }", extra_exp_rules)
    end

    def add_expire_key(key, extra_exp_rules = [])
      add_scoped_expire_key(self.class.cache_key(key), extra_exp_rules)
    end

    def add_scoped_expire_key(key, extra_exp_rules = [])
      rk = "#{ id }:rules"
      rule_keys = self.class.get_cache(rk) { [] }
      new_rule_keys = (rule_keys + extra_exp_rules + ['expire_keys']).uniq.compact.sort
      logger.debug { "add to #{ rk } keys: #{ new_rule_keys.inspect } (#{ rule_keys.length == new_rule_keys.length })" }
      self.class.set_cache(rk, new_rule_keys) unless rule_keys.length == new_rule_keys.length
      new_rule_keys.each do |rule|
        ek = "#{ id }:#{ rule }"
        exp_keys = self.class.get_cache(ek) { [] } 
        new_exp_keys = (exp_keys + [key]).uniq.sort
        logger.debug { "add to #{ ek } keys: #{ new_exp_keys.inspect } (#{ exp_keys.length != new_exp_keys.length })" }
        if exp_keys.length != new_exp_keys.length
          self.class.set_cache(ek, new_exp_keys)
        end
      end
    end

    # +exp_rules+ (an array) allow granular expiries by just 
    # expiring the specific rule instead of sweeping the cache 
    # for the entire object.  They must be specified in the 
    # +extra_exp_rules+ array when adding the expire key.
    def sweep_expire_keys(exp_rules = nil)
      exp_keys = expiry_keys(exp_rules, true).values.flatten.uniq
      logger.debug { "-- expiring keys: #{ exp_keys.inspect }" }
      exp_keys.flatten.each do |key|
        logger.debug { "==> Deleted #{ key }" }
        self.class.cache_store(:delete, key)
      end
    end

    def expiry_rules
      self.class.get_cache("#{ id }:rules") { [] }
    end
    
    def expiry_keys(exp_rules = nil, expire_rule_key = false)
      exp_rules ||= expiry_rules
      exp_rules.inject({}) do |h, rule|
        rule_key = "#{ id }:#{ rule }"
        h[rule] = self.class.get_cache(rule_key) { [] }
        self.class.expire_cache rule_key if expire_rule_key
        h
      end
    end
    
    # expires the cache and sweeps all expire keys for all rules
    def sweep_cache(rules = nil)
      expire_cache
      sweep_expire_keys(rules)
    end
  end
  
  class MarshalError < StandardError; end
end
