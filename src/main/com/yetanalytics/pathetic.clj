(ns com.yetanalytics.pathetic
  (:require [clojure.zip                         :as z]
            [clojure.spec.alpha                  :as s]
            [com.yetanalytics.pathetic.zip       :as zip]
            [com.yetanalytics.pathetic.json      :as json]
            [com.yetanalytics.pathetic.json-path :as json-path]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection from JSON data given `path`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          (let [key-path (zip/k-path loc)]
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
                    loc)))))))))))

(s/fdef get-at
  :args (s/cat :json ::json/any
               :path ::json-path)
  :ret (s/every ::json/any))

(defn get-at
  "Given json data and a parsed path, return a selection vector."
  [json path]
  (when-let [ps (try (json-path/path-seq json path)
                     (catch Exception _ nil))]
    (vary-meta (into []
                     (map second ps))
               assoc :paths (map first ps))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper fns for more complex operations on JSON data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (empty? (get-at json path))))

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
    j))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Return JSON data without whatevers at `path`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn excise
  "Given json data and a parsed path, return the data without the selection, and
  any empty container.
  If :prune-empty? is true, will remove empty arrays and maps"
  [json path & {:keys [prune-empty?
                       ]}]
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
           (set (get-at ret path)))))

(defn apply-values
  "Given json data, path and values, apply them to the structure.
  If there is no place to put a value, enumerate further possible paths and use
  those."
  [json path values & {:keys [enum-limit]}]
  ;; TODO: probably doesn't handle array slices
  (let [ps (map first (json-path/path-seq json path))
        [splice-vals append-vals] (split-at (count ps) values)
        json-spliced (reduce
                      (fn [j [p v]]
                        (assoc-in j p v))
                      json
                      (map vector
                           ps
                           splice-vals))]
    (if (not-empty append-vals)
      ;; if there are still vals to append, we should do so
      (loop [key-paths (remove
                        (partial contains? (set ps))
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
                             :values-remaining vs
                             })))
          (vary-meta j assoc :paths applied-paths)))
      ;; extra hanging paths should be removed.
      (let [spliced-count (count json-spliced)]
        (vary-meta
         (if-let [extra-paths (not-empty (drop spliced-count
                                               ps))]
           (reduce
            (partial cut true)
            json-spliced
            (reverse extra-paths))
           json-spliced)
         assoc :paths (into #{}
                            (take spliced-count
                                  ps)))))))
