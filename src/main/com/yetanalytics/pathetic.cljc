(ns com.yetanalytics.pathetic
  (:require [clojure.spec.alpha                  :as s]
            [com.yetanalytics.pathetic.json      :as json]
            [com.yetanalytics.pathetic.json-path :as json-path]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; opts-map specs

(s/def ::first? boolean?)
(s/def ::strict? boolean?)
(s/def ::return-missing? boolean?)
(s/def ::return-duplicates? boolean?)
(s/def ::prune-empty? boolean?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- assert-valid-parse
  "Throw exception if result of json-path/parse is error data."
  [res]
  (when (json-path/is-parse-failure? res)
    (throw (ex-info "Cannot parse JSONPath string"
                    (assoc res :type ::invalid-path)))))

(defn- assert-strict-valid
  "Throw exception if `strict?` is true and vector of parsed
   paths fails strict mode."
  [strict? paths]
  (when strict?
    (when-let [strict-elem (some json-path/test-strict-path paths)]
      (throw (ex-info "Illegal path element in strict mode"
                      {:type    ::invalid-strict-path
                       :paths   paths
                       :element strict-elem})))))

(defn- filter-missing
  "If filter? is true, then return only path sequences where the
   entire path has been exhausted, i.e. a value can be found at
   the JSONPath location."
  [filter? path-seqs]
  (if filter?
    (filter (complement :fail) path-seqs)
    path-seqs))

(defn- int-maps->vectors [m]
  (cond (or (not (coll? m)) (empty? m))
        m
        (vector? m)
        (mapv int-maps->vectors m)
        :else
        (let [ks (keys m)]
          (if (int? (first ks)) ;; Optimization: assume keys are homogenous
            ;; Turn int-to-val maps back into vectors
            (->> (sort ks)
                 (reduce (fn [acc k] (conj acc (get m k))) [])
                 (mapv int-maps->vectors))
            ;; Regular map; recurse over values
            (reduce-kv (fn [acc k v] (assoc acc k (int-maps->vectors v)))
                       {}
                       m)))))

(defn- prune
  "Remove empty vectors and arrays, as well as key-val pairs where the
   val is empty."
  [node]
  (letfn [(empty-coll? [x] (and (coll? x) (empty? x)))]
    (cond (map? node)
          (reduce-kv (fn [acc k v]
                       (let [v' (prune v)]
                         (if-not (empty-coll? v') (assoc acc k v') acc)))
                     {}
                     node)
          (vector? node)
          (filterv (complement empty-coll?)
                   (map prune node))
          :else
          node)))

(defn- remove-nth
  [coll n]
  (if-not (empty? coll)
    (vec (concat (subvec coll 0 n) (subvec coll (inc n))))
    coll))

(defn- cmp-seqs
  "Sort sequences lexicographically, such that:
   - shorter seqs come after longer seqs
   - strings come after ints
   This is the reverse of the default `compare` fn on vectors."
  [seq-1 seq-2]
  (loop [s1 seq-1 s2 seq-2]
    (let [x1 (first s1)
          x2 (first s2)]
      (cond
        (and (some? x1) (some? x2))
        (cond (and (string? x1) (int? x2)) 1
              (and (int? x1) (string? x2)) -1
              (= x1 x2) (recur (rest s1) (rest s2))
              :else (* -1 (compare x1 x2)))
        (and (nil? x1) (nil? x2)) 0
        (some? x1) -1
        (some? x2) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Originally "parse"

(defn parse-paths
  "Given a JSONPath string `paths`, parse the string. Each parsed
   path is a vector of the following elements:
     '..     recursive descent operator
     '*      wildcard operator
     [...]   a vector of strings (keys), integers (array indices), or
             maps (array slicing operations).
   
   The following `opts-map` fields are supported:
     :first?   Return a singleton vector containing the first path
               when multiple paths are joined using \"|\". Default
               false.
     :strict?  If true, disallows recursive descent, array slicing,
               and negative indices. Conformant to the xAPI Profile
               spec. Default false."
  ([paths]
   (parse-paths paths {}))
  ([paths opts-map]
   (let [{:keys [first? strict?]
          :or   {first? false strict? false}}
         opts-map
         res
         (json-path/parse paths)]
     (assert-valid-parse res)
     (assert-strict-valid strict? res)
     (if first? (subvec res 0 1) res))))

;; Originally "enumerate"

(s/fdef get-paths*
  :args (s/cat :json ::json/json
               :paths ::json-path/paths
               :opts-map (s/? (s/keys :opt-un [::return-missing?])))
  :ret (s/every ::json/path))

(defn get-paths*
  "Like `get-paths` except that the `paths` argument is a vector
   of already-parsed JSONPaths.
   
   The following `opts-map` fields are supported:
     :return-missing?  Return partial paths for paths that cannot
                       match any location in `json`. Default false."
  ([json paths]
   (get-paths* json paths {}))
  ([json paths opts-map]
   (let [{:keys [return-missing?] :or {return-missing? false}}
         opts-map
         enum-paths
         (fn [path]
           (->> (json-path/path-seqs json path)
                (filter-missing (not return-missing?))
                (mapv :path)))]
     (->> paths (mapcat enum-paths) distinct vec))))

(defn get-paths
  "Given `json` and a JSONPath string `paths`, return a vector of
   definite key paths. Each key path is a vector of strings (keys)
   or integers (array indices); non-deterministic path entries like
   recursive descent and wildcards are removed. If the string
   contains multiple JSONPaths, we return the key paths for all
   strings.
   
   The following `opts-map` fields are supported:
     :first?           Only apply the first \"|\"-separated path.
     :strict?          Disallow recursive descent, array slicing,
                       and negative array indices.
     :return-missing?  Return partial paths for paths that cannot
                       match any location in `json`. Default false."
  ([json paths]
   (get-paths* json (parse-paths paths)))
  ([json paths opts-map]
   (get-paths* json (parse-paths paths opts-map) opts-map)))

;; Originally "get-at"

(s/fdef get-values*
  :args (s/cat :json ::json/json
               :paths ::json-path/paths
               :opts-map (s/? (s/keys :opt-un [::return-missing?
                                               ::return-duplicates?])))
  :ret (s/every ::json/json :kind vector?))

(defn get-values*
  "Like `get-values` except that the `paths` argument is a vector
   of already-parsed JSONPaths.
   
   The following `opts-map` fields are supported:
     :return-missing?     Return values that cannot be found in `json`
                          data as nil. Default false.
     :return-duplicates?  Return duplicate values in the result.
                          Default true."
  ([json paths]
   (get-values* json paths {}))
  ([json paths opts-map]
   (let [{:keys [return-missing? return-duplicates?]
          :or   {return-missing? false return-duplicates? true}}
         opts-map
         enum-jsons
         (fn [path]
           (->> (json-path/path-seqs json path)
                (filter-missing (not return-missing?))
                (mapv :json)))
         remove-dupes
         (if return-duplicates? identity distinct)]
     (->> paths (mapcat enum-jsons) remove-dupes vec))))

(defn get-values
  "Given `json` and a JSONPath string `paths`, return a vector of
   JSON values. If the string contains multiple JSONPaths, we return
   the union of all these values.
   
   The following `opts-map` fields are supported:
     :first?              Only apply the first \"|\"-separated path.
     :strict?             Disallow recursive descent, array slicing,
                          and negative array indices.
     :return-missing?     Return values that cannot be found in `json`
                          as nil. Default false.
     :return-duplicates?  Return duplicate values in the result.
                          Default true."
  ([json paths]
   (get-values* json (parse-paths paths)))
  ([json paths opts-map]
   (get-values* json (parse-paths paths opts-map) opts-map)))

;; Formerly "path->data"

(s/fdef get-path-value-map*
  :args (s/cat :json ::json/json
               :paths ::json-path/paths
               :opts-map (s/? (s/keys :opt-un [::return-missing?])))
  :ret (s/every-kv ::json/path ::json/json))

(defn get-path-value-map*
  "Like `get-path-value-map` except that the `paths` argument is a
   vector of already-parsed JSONPaths.
   
   The following `opts-map` fields are supported:
     :return-missing?  Return path-value pairs where the path cannot
                       match any location in the `json` The result val
                       is returned as nil. Default false."
  ([json paths]
   (get-path-value-map* json paths {}))
  ([json paths opts-map]
   (let [{:keys [return-missing?] :or {return-missing? false}}
         opts-map
         enum-json-kv
         (fn [path]
           (->> path
                (json-path/path-seqs json)
                (filter-missing (not return-missing?))
                (reduce (fn [acc {jsn :json pth :path}] (assoc! acc pth jsn))
                        (transient {}))
                persistent!))]
     (->> paths
          (map enum-json-kv)
          (reduce (fn [acc m] (merge acc m)) {})))))

(defn get-path-value-map
  "Given `json` nd a JSONPath string `paths`, return a map associating
   JSON paths to JSON values. Does not return duplicates.
   
   The following `opts-map` fields are supported:
     :first?           Only apply the first \"|\"-separated path.
     :strict?          Disallow recursive descent, array slicing,
                       and negative array indices.
     :return-missing?  Return path-value pairs where the path cannot
                       match any location in the `json` The result val
                       is returned as nil. Default false."
  ([json paths]
   (get-path-value-map* json (parse-paths paths)))
  ([json paths opts-map]
   (get-path-value-map* json (parse-paths paths opts-map) opts-map)))

;; select-keys-at

(s/fdef select-keys-at*
  :args (s/cat :json ::json/json
               :paths ::json-path/paths)
  :ret ::json/json)

(defn select-keys-at*
  "Like `select-keys-at` except that the `paths` argument is a vector
   of already-parsed JSONPaths.
   
   Does not support an `opts-map` argument."
  [json paths]
  (letfn [(enum-maps
            [path]
            (reduce (fn [json {jsn :json pth :path}]
                      (if (nil? jsn)
                        (json/jassoc-in json (butlast pth) {})
                        (json/jassoc-in json pth jsn)))
                    {}
                    (json-path/path-seqs json path)))]
    (->> paths (map enum-maps) (mapv int-maps->vectors))))

(defn select-keys-at
  "Given `json` and a JSONPath string `paths`, return a map or
   vector of maps representing the key path into `json`. If the string
   contains multiple JSONPaths, we return the maps for all strings.
   If no value exists at the selection, return a truncated map with
   \"{}\" as the innermost possible value.
   
   The following `opts-map` fields are supported:
     :first?   Returns only the map corresponding to the first
               \"|\"-separated path. Default false.
     :strict?  Disallow recursive descent, array slicing, and negative
               array indices. Default false."
  ([json paths]
   (select-keys-at json paths {}))
  ([json paths opts-map]
   (let [res (select-keys-at* json (parse-paths paths opts-map))]
     (if (:first? opts-map) (first res) res))))

;; excise

(s/fdef excise*
  :args (s/cat :json ::json/json
               :paths ::json-path/paths
               :opts-map (s/? (s/keys :opt-un [::prune-empty?])))
  :ret ::json/json)

(defn excise*
  "Like `excise` except that the `paths` argument is a vector of
   already-parsed JSONPaths.
   
   The following `opts-map` fields are supported:
     :prune-empty?  Removes empty maps and vectors, as well as
                    key-value pairs where values are empty, after the
                    elements are excised. Default false."
  ([json paths]
   (excise* json paths {}))
  ([json paths opts-map]
   (let [{:keys [prune-empty?] :or {prune-empty? false}}
         opts-map
         prune-fn
         (if prune-empty? prune identity)
         rm-fn
         (fn [coll k]
           (if (int? k)
             (remove-nth coll k)
             (dissoc coll k)))
         paths'
         (->> paths
              (map (partial json-path/path-seqs json))
              (apply concat)
              ;; Don't excise failed paths
              (filterv (complement :fail))
              (map :path)
              ;; Remove identical paths to avoid out-of-bounds errors
              distinct
              ;; Sort so higher-index vector entries are removed before low-index ones
              (sort cmp-seqs))]
     (prune-fn
      (reduce (fn [json path]
                (when-not (empty? path) ;; Return nil for "$"
                  (let [last-key (last path)
                        rem-keys (butlast path)]
                    (if (empty? rem-keys)
                      (rm-fn json last-key) ;; update-in fails on empty key-paths
                      (update-in json rem-keys rm-fn last-key)))))
              json
              paths')))))

(defn excise
  "Given `json` and a JSONPath string `paths`, return the JSON value with
   the elements at the location removed.
   
   The following `opts-map` fields are supported:
     :first?        Only apply the first \"|\"-separated path.
     :strict?       Disallow recursive descent, array slicing, and
                    negative array indices.
     :prune-empty?  Removes empty maps and vectors, as well as
                    key-value pairs where values are empty, after the
                    elements are excised. Default false."
  ([json paths]
   (excise* json (parse-paths paths)))
  ([json paths opts-map]
   (excise* json (parse-paths paths opts-map) opts-map)))

;; Changed from "apply-values" to only accept one value

(s/fdef apply-value*
  :args (s/cat :json ::json/json
               :paths ::json-path/strict-paths
               :value ::json/json)
  :ret ::json/json)

(defn apply-value*
  "Like `apply-value` except that the `paths` argument is a vector of
   already-parsed JSONPaths.
   
   Does not support an `opts-map` argument."
  [json paths value]
  (let [paths' (->> paths
                    (map (partial json-path/speculative-path-seqs json))
                    (apply concat)
                    (mapv :path))]
    (reduce (fn [json path] (json/jassoc-in json path value))
            json
            paths')))

(defn apply-value
  "Given `json`, a JSONPath string `paths`, and the JSON data
   `value`, apply `value` to the location given by `paths` If
   the location exists, update the pre-existing value. Otherwise,
   create the necessary data structures needed to contain `value`.

   The following caveats apply:
   - If only the root \"$\" is provided, `json` is overwritten in
     its entirety.
   - If an array index skips over any vector entries, those skipped
     entries will be assigned nil.
   - If a path contains a wildcard and the location up to that
     point does not exist, create a new vector.
   - If a path contains a wildcard and the location is a collection,
     append it to the coll. In the case of maps, the key is its
     current size, e.g. {\"2\" : \"foo\"}.
   - Recursive descent, array slicing, and negative array indices
     are disallowed (as per strict mode).
   
   The following `opts-map` fields are supported:
     :first?   Apply only the first \"|\"-separated path. Default
               false.
     :strict?  If provided, always overrides to true."
  ([json paths value]
   (apply-value* json (parse-paths paths {:strict? true}) value))
  ([json paths value opts-map]
   (apply-value* json (parse-paths paths (assoc opts-map :strict? true)) value)))
