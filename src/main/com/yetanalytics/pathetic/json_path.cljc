(ns com.yetanalytics.pathetic.json-path
  (:require [clojure.spec.alpha     :as s]
            [clojure.spec.gen.alpha :as sgen]
            [com.yetanalytics.pathetic.json :as json]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ===== AST (post-parse) =====
;; jsonpaths      := jsonpath+
;; jsonpath       := element*
;; element        := recursive? keyset | recursive? wildcard
;; keyset         := [int-literal | string-literal | array-slice ...]
;; 
;; array-slice    := {:start int-literal | :vec-lower | :vec-higher
;;                    :end   int-literal | :vec-lower | :vec-higher
;;                    :step  int-literal}
;; int-literal    := [<integer literal>]
;; string-literal := [<string literal>]
;; wildcard       := '*
;; recursive      := '..

(defn- valid-slice-limits?
  "Returns false if the conformed slice limits are nonsensical based
   on the sign of the step."
  [{:keys [start end step]}]
  (let [[_ start] start [_ end] end]
    (if (nat-int? step)
      (and (not= :vec-higher start) (not= :vec-lower end))
      (and (not= :vec-lower start) (not= :vec-higher end)))))

(defn- valid-recursive-descent?
  "Returns false if the path vector does not have a '.. symbol followed
   by no children or another '.., true otherwise."
  [path]
  ;; Could be made cleaner using seq combinators but this is a quick and
  ;; dirty solution.
  (loop [path path]
    (if-let [elem (first path)]
      (if (= '.. elem)
        (if-let [elem' (second path)]
          (if (not= '.. elem')
            (recur (rest (rest path)))
            false)
          false)
        (recur (rest path)))
      true)))

;; Non-strict paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::wildcard #{'*})
(s/def ::recursive #{'..})

(s/def :slice/start (s/or :index int? :limit #{:vec-higher :vec-lower}))
(s/def :slice/end (s/or :index int? :limit #{:vec-higher :vec-lower}))
(s/def :slice/step int?)
(s/def ::slice (s/and (s/keys :req-un [:slice/start
                                       :slice/end
                                       :slice/step])
                      valid-slice-limits?))

(s/def ::keyset
  (s/every (s/or :key string? :index int? :slice ::slice)
           :type vector?
           :min-count 1
           :gen-max 5))

(s/def ::element
  (s/or :keyset ::keyset
        :wildcard ::wildcard
        :recursive ::recursive))

(s/def ::path
  (s/and (s/every ::element
                  :type vector?)
         valid-recursive-descent?))

(s/def ::paths
  (s/every ::path
           :type vector?
           :min-count 1))

;; Strict paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::strict-keyset
  (s/every (s/or :key string?
                 ;; Need to limit generated index size
                 :index (s/with-gen nat-int? #(sgen/choose 0 1000)))
           :type vector?
           :min-count 1
           :gen-max 5))

(s/def ::strict-element
  (s/or :keyset   ::strict-keyset
        :wildcard ::wildcard))

(s/def ::strict-path
  (s/every ::strict-element
           :type vector?
           ;; We need to limit gen here since too many wildcards can lead to
           ;; exponential blowup (25^3 = 15,625)
           :gen-max 3))

(s/def ::strict-paths
  (s/every ::strict-path
           :type vector?
           :min-count 1
           :gen-max 5))

;; Function Args ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::wildcard-append?
  boolean?)

(s/def ::wildcard-limit ; Need `choose` to limit generated int size
  (s/nilable (s/with-gen int? #(sgen/choose -5 25))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path enumeration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- init-queue [init]
  #?(:clj (conj clojure.lang.PersistentQueue/EMPTY init)
     :cljs (conj cljs.core/PersistentQueue.EMPTY init)))

(defn- throw-illegal-element [element strict?]
  (throw (ex-info (str "Illegal path element: " element)
                  {:type    ::illegal-path-element
                   :strict? strict?
                   :element element})))

(defn- count-safe
  "Like `count` but returns 0 instead of throwing if `coll` is a scalar."
  [coll]
  (if (coll? coll) (count coll) 0))

(defn- get-safe
  "Get a value only if `coll` is an actual coll (i.e. not a string)."
  [coll k]
  (when (coll? coll) (get coll k)))

;; Regular Path Enumeration Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpers

(defn- clamp-start [len idx]
  (cond (< idx 0)   0
        (> idx len) len
        :else       idx))

(defn- clamp-end [len idx]
  (cond (< idx -1)  -1
        (> idx len) len
        :else       idx))

(defn- normalize-start [len start]
  (cond (= :vec-lower start) 0
        (= :vec-higher start) (dec len)
        (neg-int? start) (clamp-start len (+ len start))
        :else (clamp-start len start)))

(defn- normalize-end [len end]
  (cond (= :vec-lower end)  -1
        (= :vec-higher end) len
        (neg-int? end)      (clamp-end len (+ len end))
        :else               (clamp-end len end)))

(defn- slice->range [json slice]
  (let [{:keys [start end step]} slice
        len   (count json)
        start (normalize-start len start)
        end   (normalize-end len end)
        ;; no infinite loops
        step  (if (zero? step) 1 step)]
    (range start end step)))

(defn- normalize-elements*
  [elements json-loc]
  (let [normalize-elements**
        (fn [normalized element]
          (cond
            ;; JSONPath element is an array splice
            (map? element)
            (if (vector? json-loc)
              (reduce (fn [keys** v] (conj! keys** v))
                      normalized
                      (slice->range json-loc element))
              ;; JSON data is not a vector, return a dummy index such that
              ;; (get json 0) => nil
              (conj! normalized 0))
            ;; JSONPath element is a negative array index
            ;; Don't clamp normalized neg indices to 0; out of bounds = `nil`
            (neg-int? element)
            (conj! normalized (+ (count json-loc) element))
            ;; JSONPath element is a regular key
            :else
            (conj! normalized element)))]
    (->> elements
         (reduce normalize-elements** (transient []))
         persistent!)))

(defn- map-or-neg-int? [x]
  (or (map? x)
      (neg-int? x)))

(defn- normalize-elements
  "Normalize element indices by doing the following:
   - Turn array splices into array index sequences
   - Turn negative array indices into positive ones"
  [elements json-loc]
  ;; Optimization: don't normalize if there's only one key or index, and
  ;; it is not a splice or negative int - the vast majority of cases.
  (if (and (= 1 (count elements))
           (not (map-or-neg-int? (peek elements))))
    elements
    (normalize-elements* elements json-loc)))

;; Main Function

(s/def ::fail boolean?)

(s/fdef path-seqs
  :args (s/cat :json ::json/json :path ::path)
  :ret  (s/every (s/keys :req-un [::json/json ::json/path ::fail])
                 :kind vector?))

(defn path-seqs
  "Given a JSON object and a parsed JSONPath, return a seq of
   maps with the following fields:
     :json  the JSON value at the JSONPath location.
     :path  the definite JSONPath that was traversed.
     :fail  if the JSONPath traversal failed due to missing
            keys or indices"
  [json-obj json-path]
  ;; Additional internal fields:
  ;;   :rest  the remaining JSONPath that could not be traversed
  ;;   :desc  if traversal was the direct result of the .. operator
  (loop [worklist (init-queue {:json json-obj
                               :rest json-path
                               :path []
                               :desc false})
         reslist  (transient [])]
    (if-let [{jsn :json rst :rest pth :path dsc :desc} (peek worklist)]
      (if-let [element (first rst)]
        ;; We order the conditions heuristically based on likelihood of
        ;; encounter, in order to minimize equivalent operations
        (cond
          ;; Short circuit: if json is a primitive or `nil` stop traversal
          (not (coll? jsn))
          (let [worklist' (pop worklist)
                reslist'  (cond-> reslist
                            (not dsc) ; Mark failure if not recursive descent
                            (conj! {:json nil
                                    :path pth
                                    :fail true}))]
            (recur worklist' reslist'))
          
          ;; Vector of keys/indices/slices
          (vector? element)
          (let [element'
                (normalize-elements element jsn)
                [worklist' reslist']
                (reduce
                 (fn [[worklist reslist] sub-element]
                   (cond
                     ;; Element exists in JSON location
                     ;; Need to separate `contains?` from `get` to distinguish
                     ;; nil values from non-existent ones in the coll.
                     (contains? jsn sub-element)
                     [(conj worklist {:json (get-safe jsn sub-element)
                                      :rest (rest rst)
                                      :path (conj pth sub-element)
                                      :desc false})
                      reslist]
                     ;; Recursive desc: don't add missing keys
                     dsc
                     [worklist reslist]
                     ;; Otherwise, mark failure
                     :else
                     [worklist
                      (conj! reslist {:json nil
                                      :path (conj pth sub-element)
                                      :fail true})]))
                 [(pop worklist) reslist]
                 element')]
            (recur worklist' reslist'))
          
          ;; Wildcard
          (= '* element)
          (let [[worklist' reslist']
                (if (zero? (count jsn))
                  ;; No children in coll; mark failure
                  [(pop worklist)
                   (conj! reslist {:json nil
                                   :path pth
                                   :fail true})]
                  ;; Children in coll; continue
                  [(reduce-kv
                    (fn [worklist key child]
                      (conj worklist {:json child
                                      :rest (rest rst)
                                      :path (conj pth key)
                                      :desc false}))
                    (pop worklist)
                    jsn)
                   reslist])]
            (recur worklist' reslist'))
          
          ;; Recursive descent
          (= '.. element)
          (let [desc-list (json/recursive-descent jsn)
                worklist' (reduce
                           (fn [worklist desc]
                             (conj worklist
                                   {:json (:json desc)
                                    :rest (rest rst)
                                    :path (->> desc :path (concat pth) vec)
                                    :desc true}))
                           (pop worklist)
                           desc-list)]
            (recur worklist' reslist))
          
          ;; Unknown path element
          :else
          (throw-illegal-element element false))
        ;; Path is exhausted; move item from worklist to reslist
        (let [worklist' (pop worklist)
              reslist'  (conj! reslist {:json jsn
                                        :path pth
                                        :fail false})]
          (recur worklist' reslist')))
      (persistent! reslist))))

;; Speculative Path Enumeration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpers

(defn- wildcard-indexes
  "At a given `json` location, determine the indexes of the speculative
   path at that location. In append mode, return up to `wildcard-limit`
   indexes (defaults to `1` if `nil`). In overwrite mode, return up to
   either `wildcard-limit` or `(count jsn)` indexes."
  [jsn wildcard-append? wildcard-limit]
  (if wildcard-append?
    ;; append mode
    (cond->> (range (count-safe jsn)
                    (+ (count-safe jsn)
                       (or wildcard-limit 1)))
      (map? jsn) (map str))
    ;; overwrite mode
    (cond
      (map? jsn)
      (cond->> (sort (keys jsn))
        wildcard-limit (take wildcard-limit))
      (coll? jsn)
      (range (or wildcard-limit
                 (count-safe jsn)))
      :else [0])))

;; Main function

(s/fdef speculative-path-seqs
  :args (s/cat :json ::json/json
               :path ::strict-path
               :wildcard-append? ::wildcard-append?
               :wildcard-limit   ::wildcard-limit)
  :ret  (s/every (s/keys :req-un [::json/json ::json/path])
                 :kind vector?))

(defn speculative-path-seqs
  "Similar to `path-seqs`, except it continues traversing the path even if
   the location in the JSON data is missing or incompatible. Returns the
   same fields as path-seqs except for `:fail`.
   
   Accepts two more args: `wildcard-append?`, which dictates if wildcard values
   should be appended to the end of existing seqs (default `true`), and
   `wildcard-limit`, dictating how many wildcard paths should be generated
   (defaults to 1 in append mode, the coll size in overwrite mode).
   
   Note that too many wildcards in `json-path`, or too large of a value of
   `wildcard-limit`, may lead to exponential blowup."
  [json-obj json-path wildcard-append? wildcard-limit]
  (loop [worklist (init-queue {:json json-obj
                               :rest json-path
                               :path []})
         reslist  (transient [])]
    (if-let [{jsn :json rst :rest pth :path} (peek worklist)]
      (if-let [element (first rst)]
        (cond
          ;; Vector of keys/indices
          (vector? element)
          (let [worklist' (reduce
                           (fn [worklist sub-element]
                             (if (or (string? sub-element)
                                     (nat-int? sub-element))
                               (conj worklist
                                     {:json (get-safe jsn sub-element)
                                      :rest (rest rst)
                                      :path (conj pth sub-element)})
                               ;; Non-strict elements like neg indices or slices
                               (throw-illegal-element element true)))
                           (pop worklist)
                           element)]
            (recur worklist' reslist))

          ;; Wildcard
          (= '* element)
          (let [indexes   (wildcard-indexes jsn wildcard-append? wildcard-limit)
                worklist' (reduce
                           (fn [worklist idx]
                             (conj worklist
                                   {:json nil ; current val doesn't matter
                                    :rest (rest rst)
                                    :path (conj pth idx)}))
                           (pop worklist)
                           indexes)]
            (recur worklist' reslist))

          ;; Unknown path element
          ;; Includes recursive descent and other non-strict elements
          :else
          (throw-illegal-element element true))
        ;; Path is exhausted; move item from worklist to reslist
        (let [worklist' (pop worklist)
              reslist'  (conj! reslist {:json jsn
                                        :path pth})]
          (recur worklist' reslist')))
      (persistent! reslist))))
