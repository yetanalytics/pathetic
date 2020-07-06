(ns com.yetanalytics.pathetic.json-path
  (:require [blancas.kern.core :as k]
            [clojure.spec.alpha :as s]
            [blancas.kern.lexer.basic :as kl]
            [clojure.math.combinatorics :as combo]
            [com.yetanalytics.pathetic.json :as json]))

(s/def ::root
  #{'$})

(def root
  (k/<$>
   (constantly '$)
   (k/sym* \$)))

(s/def ::wildcard
  #{'*})

(def wildcard
  (k/<$>
   (constantly '*)
   (k/sym* \*)))

(s/def ::keyset
  (s/or :keys (s/every string?
                       :type set?
                       :into #{}
                       :min-count 1)
        :indices (s/every int?
                          :type set?
                          :into #{}
                          :min-count 1)))

(def index
  (k/<$>
   (fn [^String x]
     (Long/parseLong x))
   (k/<+>
    (k/optional (k/sym* \-))
    kl/dec-lit)))

(s/def :range/start
  int?)

(s/def :range/end
  int?)

(s/def :range/step
  int?)

(s/def :range/bounded? ;; was this range bounded, or does it use a MAX_VALUE?
  boolean?)



(defrecord RangeSpec [start end step bounded?])

(s/fdef range-spec?
  :args (s/cat :item any?)
  :ret boolean?)

(defn range-spec?
  [item]
  (instance? RangeSpec item))

(s/def ::range
  (s/keys :req-un [:range/start
                   :range/end
                   :range/step
                   :range/bounded?]))

(def ^:const max-long-str
  (str Long/MAX_VALUE))

(def index-range
  (k/bind [start (k/option 0
                           index)
           _ kl/colon
           end (k/option Long/MAX_VALUE
                         index)
           _ (k/optional kl/colon)
           step (k/option 1
                          index)]
          (k/return
           (->RangeSpec (if (number? start)
                          start
                          (Long/parseLong start))
                        (if (number? end)
                          end
                          (Long/parseLong end))

                        (if (number? step)
                          step
                          (Long/parseLong step))
                        ;; if the option is used, we're unbounded
                        (if (#{Long/MAX_VALUE
                               max-long-str} end)
                          false
                          true)))))

(defn escaped-by
  [c & [charset-p]]
  (k/>> (k/sym* c)
        (or charset-p k/any-char)))

(def escaped-char
  (escaped-by (char 92)))

(def safe-char
  (k/none-of* (str
               ;; double-quote
               (char 34)
               ;; single quote
               (char 39)
               ;; escape char
               (char 92))))

(def json-key
  (k/<+>
   (k/many
    (k/<|>
     escaped-char
     safe-char))))

(def json-key-lit
  (k/<+>
   (k/between (k/sym* \') json-key)))

(defn union-of1
  "A comma separated union of at least 1 of something.
   Returns a set."
  [p]
  (k/<$>
   set
   (k/sep-by1 (k/sym* \,) p)))

(def child-normal
  "Normal bracket-style child"
  (kl/brackets
   (k/<|>
    wildcard
    (k/<:> index-range)
    (union-of1 index)
    (union-of1 json-key-lit))))

(def child-dot
  "Stupid dot syntax"
  (k/>>
   kl/dot
   (k/<|>
    ;; normal key
    (k/<$>
     (partial hash-set)
     (k/<+> (k/many1 k/alpha-num)
            ;; support full language tags
            (k/optional (k/<+> (k/one-of* "-")
                               (k/many1 k/alpha-num)))))
    ;; dot wildcard
    wildcard
    ;; double-dot wildcard
    (k/<$>
     (constantly '*)
     (k/look-ahead
      (k/>> kl/dot
            (k/many1 k/alpha-num)))))))


(def json-path
  (k/>> root
        (k/many (k/<|>
                 child-normal
                 child-dot))))

(s/def ::json-path
  (s/every (s/or :keyset ::keyset
                 :wildcard ::wildcard
                 :range ::range)))

(s/fdef parse
  :args (s/cat :path string?)
  :ret ::json-path)

(defn parse
  "Given a JSON-path, parse it into data"
  [path]
  (:value (k/parse json-path
                   path)))

;; found in datassim, may need to be elsewhere

(s/def :enumerate/limit
  number?)

(s/fdef enumerate
  :args (s/cat :path ::json-path
               :options (s/keys* :opt-un [:enumerate/limit]))
  :ret (s/every ::json/key-path))

(defn enumerate
  "Given a json path, return a lazy seq of concrete key paths. wildcards/ranges
  will be enumerated up to :limit, which defaults to 10"
  [path & {:keys [limit]}]
  (map vec
       (apply combo/cartesian-product
              (map
               (fn [element]
                 (cond
                   (set? element)
                   element

                   (= '* element)
                   (if limit
                     (range limit)
                     (range))
                   ;; otherwise, itsa range spec
                   :else
                   (let [{:keys [start end step]} element]
                     (cond->> (range start end step)
                       limit (take limit)))))
               path))))

(s/fdef path-seq
  :args (s/cat :json ::json/any
               :path ::json-path)
  :ret (s/every (s/tuple ::json/key-path
                         ::json/any)))

(defn path-seq*
  [json path]
  (lazy-seq
   (let [path-enum (map vec
                        (apply combo/cartesian-product
                               path))
         hits (for [p path-enum
                    :let [hit (get-in json p)]
                    :while (some? hit)]
                   [p hit])
         ]
     (when (not-empty hits)
       (concat
        hits
        ;; if the enum continues,
        (when-let [first-fail (first (drop (count hits) path-enum))]
          (let [last-pass (first (last hits))
                fail-idx
                (some
                 (fn [[idx pv fv]]
                   (when (not= pv fv)
                     idx))
                 (map vector
                      (range)
                      last-pass
                      first-fail))]
            (when-let [[edit-idx v]
                       (and (< 0 fail-idx)
                            (some
                             (fn [idx]
                               (let [seg (get path idx)]
                                 (if (set? seg)
                                   (when (< 1 (count seg))
                                     [idx (disj seg
                                                (get first-fail
                                                     idx))])
                                   (let [re-ranged
                                         (drop-while
                                          #(<= % (get first-fail idx))
                                          seg)]
                                     (when (first re-ranged)
                                       [idx re-ranged])))))
                             (reverse (range fail-idx))))]
              (path-seq*
               json
               (assoc path edit-idx v))))))))))

(defn path-seq
  [json path]
  (path-seq* json (mapv
                   (fn [element]
                     (cond
                       (set? element)
                       element

                       (= '* element)
                       (range)
                       ;; otherwise, itsa range spec
                       :else
                       (let [{:keys [start end step]} element]
                         (range start end step))))
                   path)))

(s/fdef select
  :args (s/cat :json ::json/any
               :path ::json-path)
  :ret (s/every ::json/any))



(defn select
  "Given json data and a parsed path, return a selection vector."
  [json path]
  (let [ps (path-seq json path)]
    (vary-meta (into []
                     (map second ps))
               assoc :paths (map first ps))))

(s/fdef select-paths
  :args (s/cat :json ::json/any
               :path ::json-path)
  :ret (s/map-of ::json/key-path
                 ::json/any))

(defn select-paths
  "Given json data and a parsed path, return a selection map of key paths to values"
  [json path]
  (into {}
        (path-seq json path)))

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
        (empty? (select json path))))

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

(defn excise
  "Given json data and a parsed path, return the data without the selection, and
  any empty container.
  If :prune-empty? is true, will remove empty arrays and maps"
  [json path & {:keys [prune-empty?
                       ]}]
  (let [ps (path-seq json path)
        psk (map first ps)]
    (vary-meta
     (reduce
      (partial cut prune-empty?)
      json
      ;; reverse the paths so the indices stay correct!
      ;; TODO: probably doesn't handle array slices
      (reverse psk))
     assoc :paths (into #{} psk))))

(s/fdef discrete?
  :args (s/cat :path ::json-path)
  :ret boolean?)

(defn discrete?
  "Is the path free of wildcards?"
  [path]
  (not
   (some (fn [x]
           (or (= '* x)
               (and (range-spec? x)
                    (not (:bounded? x)))))
         path)))

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
           (set (select ret path)))))

(defn apply-values
  "Given json data, path and values, apply them to the structure.
  If there is no place to put a value, enumerate further possible paths and use
  those."
  [json path values & {:keys [enum-limit]}]
  ;; TODO: probably doesn't handle array slices
  (let [ps (map first (path-seq json path))
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
                        (enumerate
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
