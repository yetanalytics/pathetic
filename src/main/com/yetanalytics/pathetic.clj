(ns com.yetanalytics.pathetic
  (:require [clojure.zip                         :as z]
            [clojure.spec.alpha                  :as s]
            [clojure.set :as cset]
            [clojure.core.match :as m]
            [clojure.walk :as w]
            [com.yetanalytics.pathetic.zip       :as zip]
            [com.yetanalytics.pathetic.json      :as json]
            [com.yetanalytics.pathetic.json-path :as json-path]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection from JSON data given `path`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef select-keys-at
  :args (s/cat :path string?
               :json ::json/any)
  :ret ::json/any)

(comment
  (defn select-keys-at
    "Given json data and a path, return the selection. Note that this does not
  return the json-path selection, just the pruned datastructure as with
  clojure.core/select-keys"
    [json path]
    (let [path (json-path/parse path)]
      (loop [loc (zip/json-zip json)]
        (if (z/end? loc)
          (z/root loc)
          (if (zip/internal? loc)
            (recur (z/next loc))
            (let [key-path (zip/key-path loc)]
              (if-let [sat (json-path/satisfied path key-path)]
              ;; if we have satisfied at least some of the spec, we
              ;; want to keep going
                (recur (z/next loc))
              ;; if it doesn't match, kill this or any internal nodes
              ;; leading to it.
                (let [ploc (z/up loc)
                      pnode (z/node ploc)]
                  (recur
                   (z/remove
                    (if (map-entry? pnode)
                      ploc
                      loc))))))))))))

(s/fdef get-at
  :args (s/cat :json ::json/any
               :path ::json-path)
  :ret (s/every ::json/any))

(comment
  (defn get-at
    "Given json data and a parsed path, return a selection vector."
    [json path]
    (when-let [ps (try (json-path/path-seq json path)
                       (catch Exception _ nil))]
      (vary-meta (into []
                       (map second ps))
                 assoc :paths (map first ps)))))

(s/fdef path->data
  :args (s/cat :json ::json/any
               :path ::json-path)
  :ret (s/map-of ::json/key-path
                 ::json/any))

(defn path->data
  "Given json data and a parsed path, return a selection map of key paths to values"
  [json path]
  (into {}
        (json-path/path-seq json path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Helper fns for more complex operations on JSON data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (s/def :excise/prune-empty?
    boolean?)

  (s/fdef excise
    :args (s/cat :json ::json/any
                 :path ::json-path
                 :options (s/keys* :opt-un [:excise/prune-empty?]))
    :ret (s/every ::json/any)
    :fn (fn [{:keys [ret]
              {path :path
               json :json} :args}]
          (empty? (get-at-2 json path))))

  (defn- cut [prune-empty? j key-path]
    (if (some? (get-in j key-path))
      (if (= 1 (count key-path))
      ;; we don't prune at the top level, so this is simple
        (let [[k] key-path
              j-after (if (string? k)
                        (dissoc j k)
                        (into []
                              (let [[before [_ & after]] (split-at k j)]
                                (concat before after))))]
          j-after)
        (let [last-k (peek key-path)
              parent-key-path (into [] (butlast key-path))
              parent (get-in j parent-key-path)
              j-after (update-in j
                                 parent-key-path
                                 (partial cut prune-empty?)
                                 [last-k])]
          (if (and prune-empty?
                   (empty? (get-in j-after parent-key-path)))
            (recur prune-empty? j-after parent-key-path)
            j-after)))
      j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Return JSON data without whatevers at `path`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (defn excise
    "Given json data and a parsed path, return the data without the selection, and
  any empty container.
  If :prune-empty? is true, will remove empty arrays and maps"
    [json path & {:keys [prune-empty?]}]
    (let [ps (json-path/path-seq json path)
          psk (map first ps)]
      (vary-meta
       (reduce
        (partial cut prune-empty?)
        json
      ;; reverse the paths so the indices stay correct!
      ;; TODO: probably doesn't handle array slices
        (reverse psk))
       assoc :paths (into #{} psk))))

  (s/fdef apply-values
    :args (s/cat :json ::json/any
                 :path ::json-path
                 :values (s/every ::json/any))
    :ret (s/every ::json/any)
    :fn (fn [{:keys [ret]
              {path :path
               json :json
               values :values} :args}]
          (= (set values)
             (set (get-at-2 ret path)))))

  (defn apply-values
    "Given json data, path and values, apply them to the structure.
  If there is no place to put a value, enumerate further possible paths and use
  those."
    [json path values & {:keys [enum-limit]}]
  ;; TODO: probably doesn't handle array slices
    (let [path-seqs     (map first (json-path/path-seq json path))
          [splice-vals
           append-vals] (split-at (count path-seqs) values)
          json-spliced  (reduce (fn [j [p v]] (assoc-in j p v))
                                json
                                (map vector path-seqs splice-vals))]
      (if (not-empty append-vals)
      ;; if there are still vals to append, we should do so
        (loop [key-paths (remove
                          (partial contains? (set path-seqs))
                          (json-path/enumerate
                           path
                           :limit (or enum-limit 3)))
               vs values
               j json
               applied-paths #{}]
          (if-some [v (first vs)]
            (if-let [key-path (first key-paths)]
              (let [;; edge case check
                  ;; -> single `key-path` but many `values`
                    cur-only-path? (nil? (seq (rest key-paths)))
                    first-of-many? (and (> (count values) 1)
                                      ;; ensure caught before attempted
                                        (= v (first values)))]
                (if (and cur-only-path? first-of-many?)
                ;; FIXME: update ^ to check location makes sense for an array
                  (recur (rest key-paths)
                         [] ;; consuming all values at once, pass empty to pull out of loop
                         (json/jassoc-in j key-path vs)
                         (conj applied-paths key-path))
                  (recur (rest key-paths)
                         (rest vs)
                         (json/jassoc-in j key-path v)
                         (conj applied-paths key-path))))
              (throw (ex-info "Couldn't make enough paths"
                              {:type ::out-of-paths
                               :path path
                               :json json
                               :json-mod j
                               :values values
                               :values-remaining vs})))
            (vary-meta j assoc :paths applied-paths)))
      ;; extra hanging paths should be removed.
        (let [spliced-count (count json-spliced)]
          (vary-meta
           (if-let [extra-paths (not-empty (drop spliced-count
                                                 path-seqs))]
             (reduce
              (partial cut true)
              json-spliced
              (reverse extra-paths))
             json-spliced)
           assoc :paths (into #{}
                              (take spliced-count
                                    path-seqs))))))))

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

(defn- take-except-last
  [coll]
  (take (-> coll count dec) coll))

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

(defn parse
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
      (throw (ex-info "Cannot parse JSONPath string" res))
      (do (when strict?
            (when-let [strict-elem (if first?
                                     (json-path/get-not-strict res)
                                     (some json-path/get-not-strict res))]
              (throw (ex-info "Illegal path element in strict mode"
                              {:element strict-elem}))))
          res))))

(defn enumerate
  "Given JSON data and a JSONPath string, return a vector of
   definite key paths. Each key path is a vector of strings (keys)
   or integers (array indices); non-deterministic path entries like
   recursive descent and wildcards are removed. If the string
   contains multiple JSONPaths, we return the key paths for all
   strings.
   
   The following optional arguments are supported:
     :return-missing?  Return paths that cannot match any location
                       in the JSON object as nil. Default false."
  [json paths & {:keys [return-missing?] :or {return-missing? false}}]
  (letfn [(enum-paths [path] (->> (json-path/path-seqs json path)
                                  (filter-missing (not return-missing?))
                                  (mapv :path)))]
    (->> paths parse (mapcat enum-paths) distinct vec)))

(defn get-at
  "Given JSON data and a JSONPath string, return a vector of
   JSON values. If the string contains multiple JSONPaths, we return
   the union of all these values.
   
   The following optional arguments are supported:
     :return-missing?     Return values that cannot be found in the JSON
                          object as nil. Default false.
     :return-duplicates?  Return duplicate values in the array. Default
                          true."
  [json paths & {:keys [return-missing? return-duplicates?]
                 :or   {return-missing?    false
                        return-duplicates? true}}]
  (letfn [(enum-jsons [path] (->> (json-path/path-seqs json path)
                                  (filter-missing (not return-missing?))
                                  (mapv :json)))]
    (let [remove-dupes (if return-duplicates? identity distinct)]
      (->> paths parse (mapcat enum-jsons) remove-dupes vec))))

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
                        (assoc-in json (take-except-last pth) {})
                        (assoc-in json pth jsn)))
                    {}
                    (json-path/path-seqs json path)))]
    (if first?
      (->> (parse paths :first? true)
           (conj [])
           (map enum-maps)
           (map int-maps->vectors)
           first)
      (->> (parse paths)
           (map enum-maps)
           (map int-maps->vectors)
           vec))))

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
                      parse
                      (mapv (partial json-path/path-seqs json))
                      (apply concat) ;; Flatten coll of path seqs
                      (filterv (complement :fail)) ;; Don't excise fail paths
                      (mapv :path)
                      (sort cmp-vecs) ;; Sort/reverse so higher-index vector
                      reverse)]       ;; entries are removed before low-index ones
    (prune-fn
     (reduce (fn [json path]
               (let [last-key (last path)
                     rem-keys (take-except-last path)]
                 (if (empty? rem-keys)
                   (rm-fn json last-key) ;; update-in fails on empty key-paths
                   (update-in json rem-keys rm-fn last-key))))
             json
             paths'))))

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
  (let [paths' (->> (parse paths :strict? true)
                    (mapv (partial json-path/speculative-path-seqs json))
                    (apply concat)
                    (filterv (complement :fail))
                    (mapv :path))]
    (reduce (fn [json path] (json/jassoc-in json path value))
            json
            paths')))
