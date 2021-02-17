(ns com.yetanalytics.pathetic
  (:require [clojure.spec.alpha                  :as s]
            [com.yetanalytics.pathetic.json      :as json]
            [com.yetanalytics.pathetic.json-path :as json-path]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- filter-missing
  "If filter? is true, then return only path sequences where the
   entire path has been exhausted, i.e. a value can be found at
   the JSONPath location."
  [filter? path-seqs]
  (if filter?
    (filterv (complement :fail) path-seqs)
    path-seqs))

(defn- int-maps->vectors [m]
  (cond (or (not (coll? m)) (empty? m))
        m
        (vector? m)
        (mapv int-maps->vectors m)
        :else
        (let [ks (keys m)]
          (if (every? int? ks)
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
  (vec (concat (subvec coll 0 n)
               (subvec coll (inc n)))))

(defn- cmp-vecs
  "Sort vectors lexicographically, with strings always coming before ints."
  [vec-1 vec-2]
  (loop [v1 vec-1 v2 vec-2]
    (if-let [x1 (first v1)]
      (if-let [x2 (first v2)]
        (cond (and (string? x1) (int? x2)) -1
              (and (int? x1) (string? x2)) 1
              (= x1 x2) (recur (rest v1) (rest v2))
              :else (compare x1 x2))
        (compare v1 v2))
      (compare v1 v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Originally "parse"

(s/fdef parse-path
  :args (s/cat :path string?)
  :ret (s/or :first ::json/path :all (s/every ::json/path)))

(defn parse-path
  "Parse a JSONPath string. Each parsed path is a vector with the
   following entries:
     '..     recursive descent operator
     '*      wildcard operator
     [...]   a vector of strings (keys), integers (array indices), or
             maps (array slicing operations).
   
   The following optional arguments are supported:
     :first?   Return the first path when multiple paths are joined
               using the \"|\" operator. Default false (in which case
               a vector of paths is returned).
     :strict?  If true, disallows recursive descent, array slicing,
               and negative indices. Conformant to the xAPI Profile
               spec and used by apply-values. Default false."
  [paths & {:keys [first? strict?]
            :or {first? false strict? false}}]
  (let [res (if first?
              (json-path/parse-first paths)
              (json-path/parse paths))]
    (if (json-path/is-parse-failure? res)
      (throw (ex-info "Cannot parse JSONPath string"
                      (assoc res :type ::invalid-path)))
      (do (when strict?
            (when-let [strict-elem (if first?
                                     (json-path/get-not-strict res)
                                     (some json-path/get-not-strict res))]
              (throw (ex-info "Illegal path element in strict mode"
                              {:type    ::invalid-strict-path
                               :paths   res
                               :element strict-elem}))))
          res))))

;; Originally "enumerate"

(s/fdef get-paths
  :args (s/cat :json ::json/json :paths string?)
  :ret (s/every ::json/path))

(defn get-paths
  "Given JSON data and a JSONPath string, return a vector of
   definite key paths. Each key path is a vector of strings (keys)
   or integers (array indices); non-deterministic path entries like
   recursive descent and wildcards are removed. If the string
   contains multiple JSONPaths, we return the key paths for all
   strings.
   
   The following optional arguments are supported:
     :return-missing?  Return paths that cannot match any location
                       in the JSON data as nil. Default false."
  [json paths & {:keys [return-missing?] :or {return-missing? false}}]
  (letfn [(enum-paths [path] (->> (json-path/path-seqs json path)
                                  (filter-missing (not return-missing?))
                                  (mapv :path)))]
    (->> paths parse-path (mapcat enum-paths) distinct vec)))

;; Originally "get-at"

(s/fdef get-values
  :args (s/cat :json ::json/json :paths string?)
  :ret (s/every ::json/any))

(defn get-values
  "Given JSON data and a JSONPath string, return a vector of
   JSON values. If the string contains multiple JSONPaths, we return
   the union of all these values.
   
   The following optional arguments are supported:
     :return-missing?     Return values that cannot be found in the JSON
                          data as nil. Default false.
     :return-duplicates?  Return duplicate values in the array. Default
                          true."
  [json paths & {:keys [return-missing? return-duplicates?]
                 :or   {return-missing?    false
                        return-duplicates? true}}]
  (letfn [(enum-jsons [path] (->> (json-path/path-seqs json path)
                                  (filter-missing (not return-missing?))
                                  (mapv :json)))]
    (let [remove-dupes (if return-duplicates? identity distinct)]
      (->> paths parse-path (mapcat enum-jsons) remove-dupes vec))))

;; Formerly "path->data"

(s/fdef get-path-value-map
  :args (s/cat :json ::json/json :paths string?)
  :ret (s/map-of ::json/path ::json/json))

(defn get-path-value-map
  "Given JSON data nd a JSONPath string, return a map associating
   JSON paths to JSON values. Does not return duplicates.
   
   The following optional arguments are supported:
     :return-missing?  Return path-value pairs where the path cannot
                       match any location in the JSON data. The object
                       is returned as nil. Default false."

  [json paths & {:keys [return-missing?] :or {return-missing? false}}]
  (letfn [(enum-json-kv [path]
                        (->> (json-path/path-seqs json path)
                             (filter-missing (not return-missing?))
                             (reduce (fn [acc {jsn :json pth :path}]
                                       (assoc acc pth jsn))
                                     {})))]
    (->> paths
         parse-path
         (map enum-json-kv)
         (reduce (fn [acc m] (merge acc m)) {}))))

;; select-keys-at

(s/fdef select-keys-at
  :args (s/cat :json ::json/json :paths string?)
  :ret ::json/any)

(defn select-keys-at
  "Given JSON data and a JSONPath string, return a vector of maps
   that represent the key path into the JSON value. If the string
   contains multiple JSONPaths, we return the maps for all strings.
   If no value exists at the selection, return a truncated map with
   \"{}\" as the innermost possible value.
   
   The following optional arguments are supported:
     :first?  Returns the maps corresponding to the first path (if
              paths are separated by \"|\"). Default false."
  [json paths & {:keys [first?] :or {first? false}}]
  (letfn [(enum-maps
            [path]
            (reduce (fn [json {jsn :json pth :path}]
                      (if (nil? jsn)
                        (assoc-in json (butlast pth) {})
                        (assoc-in json pth jsn)))
                    {}
                    (json-path/path-seqs json path)))]
    (if first?
      (->> (parse-path paths :first? true)
           (conj [])
           (map enum-maps)
           (map int-maps->vectors)
           first)
      (->> (parse-path paths)
           (map enum-maps)
           (map int-maps->vectors)
           vec))))

;; excise

(s/fdef excise
  :args (s/cat :json ::json/json :paths string?)
  :ret ::json/json)

(defn excise
  "Given JSON data and a JSONPath string, return the JSON value with
   the elements at the location removed.
   
   The following optional arguments are supported:
     :prune-empty?  Removes empty maps and vectors, as well as
                    key-value pairs where values are empty, after the
                    elements are excised. Default false."
  [json paths & {:keys [prune-empty?] :or {prune-empty? false}}]
  (let [prune-fn (if prune-empty? prune identity)
        rm-fn    (fn [coll k] (if (int? k)
                                (remove-nth coll k)
                                (dissoc coll k)))
        paths'   (->> paths
                      parse-path
                      (mapv (partial json-path/path-seqs json))
                      (apply concat)               ;; Flatten coll of path seqs
                      (filterv (complement :fail)) ;; Don't excise fail paths
                      (mapv :path)
                      (sort cmp-vecs) ;; Sort/reverse so higher-index vector
                      reverse)]       ;; entries are removed before low-index ones
    (prune-fn
     (reduce (fn [json path]
               (let [last-key (last path)
                     rem-keys (butlast path)]
                 (if (empty? rem-keys)
                   (rm-fn json last-key) ;; update-in fails on empty key-paths
                   (update-in json rem-keys rm-fn last-key))))
             json
             paths'))))

;; Changed from "apply-values" to only accept one value

(s/fdef apply-value
  :args (s/cat :json ::json/json :paths string? :value ::json/json)
  :ret ::json/json)

(defn apply-value
  "Given JSON data, a JSONPath string, and a JSON value, apply the
   value to the location given by the path. If the location exists,
   update the pre-existing value. Otherwise, create the necessary
   data structures needed to contain the JSON value.

   The following caveats apply:
   - If an array index skips over any vector entries, those skipped
     entries will be assigned nil.
   - If a path contains a wildcard and the location up to that
     point does not exist, create a new vector.
   - If a path contains a wildcard and the location is a collection,
     append it to the coll. In the case of maps, the key is its
     current size, e.g. {\"2\" : \"foo\"}.
   - Recursive descent, array slicing, and negative array indices
     are disallowed (as per strict mode)."
  [json paths value]
  (let [paths' (->> (parse-path paths :strict? true)
                    (mapv (partial json-path/speculative-path-seqs json))
                    (apply concat)
                    (filterv (complement :fail))
                    (mapv :path))]
    (reduce (fn [json path] (json/jassoc-in json path value))
            json
            paths')))
