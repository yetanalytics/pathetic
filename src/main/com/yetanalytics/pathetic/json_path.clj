(ns com.yetanalytics.pathetic.json-path
  (:require [clojure.core.match :as m]
            [clojure.string :as string]
            [clojure.set :as cset]
            [clojure.walk :as w]
            [clojure.spec.alpha             :as s]
            [clojure.math.combinatorics     :as combo]
            [com.yetanalytics.pathetic.json :as json]
            #_[com.yetanlaytics.pathetic.zip  :as zip]
            [instaparse.core :as insta]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Indefinite paths

(s/def ::root #{'$})

(s/def ::wildcard #{'*})
(s/def ::recursive #{'..})

(s/def :slice/start (s/or :index int? :limit #{:vec-higher :vec-lower}))
(s/def :slice/end (s/or :index int? :limit #{:vec-higher :vec-lower}))
(s/def :slice/step int?)
(s/def ::slice (s/keys :req-un [:slice/start
                                :slice/end
                                :slice/step]))

(s/def ::keyset
  (s/every (s/or :key string? :index int? :range ::range)
           :type vector?
           :min-count 1))

(s/def ::json-path
  (s/every (s/or :keyset    ::keyset
                 :wildcard  ::wildcard
                 :recursive ::recursive)
           :type vector?))

(s/def ::json-paths (s/every ::json-path
                             :type vector?
                             :min-count 1))

;; Instaparse parsing failures

(s/def :failure/tag keyword?)
(s/def :failure/expecting any?) ;; Just need that keyword to exist
(s/def :failure/reason (s/coll-of (s/keys :req-un [:failure/tag
                                                   :failure/expecting])))
(s/def :failure/index int?)

(s/def ::parse-failure (s/keys :req-un [:failure/index
                                        :failure/reason]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Grammar inspired from:
;; https://github.com/dchester/jsonpath/blob/master/lib/grammar.js
;; 
;; Integer and string literal regexes inspired from:
;; https://github.com/dchester/jsonpath/blob/master/lib/dict.js

(def jsonpath-instaparser
  (insta/parser
   "jsonpaths  := <ws>* jsonpath (<ws>* <'|'> <ws>* jsonpath)* <ws>*;
    jsonpath   := <root> children?;
    <children> := child+;
    child      := bracket-child | dot-child;

    <bracket-child>   := double-dot? <'['> <ws*> bracket-content <ws*> <']'>;
    <bracket-content> := wildcard | bracket-union;
    bracket-union     := union-element (<ws>* <','> <ws>* union-element)*;
    <union-element>   := int-literal | string-literal | array-slice;
    array-slice       := int-literal? ':' int-literal? (':' int-literal?)?;
  
    <dot-child>   := double-dot child-body | <dot> child-body;
    <child-body>  := wildcard | identifier;
    
    identifier     := #'[a-zA-Z0-9_\\-]+';
    int-literal    := #'-?[0-9]+';
    string-literal := string-literal-sq | string-literal-dq
    <string-literal-sq> := #'\\'(?:\\\\[\\'bfnrt/\\\\]|\\\\u[a-fA-F0-9]{4}|[^\\'\\\\])*\\'';
    <string-literal-dq> := #'\"(?:\\\\[\"bfnrt/\\\\]|\\\\u[a-fA-F0-9]{4}|[^\"\\\\])*\"'

    root        := '$';
    wildcard    := '*';
    double-dot  := '..';
    dot         := '.';
    ws          := #'\\s'
  "))

;; ===== AST (post-parse) =====
;; jsonpaths      := jsonpath+
;; jsonpath       := child*
;; child          := identifier | int-literal | string-literal |
;;                   array-slice | bracket-union |
;;                   wildcard | double-dot
;; bracket-union  := #{int-literal | string-literal | array-slice ...}
;; array-slice    := {:start int-literal
;;                    :end int-literal
;;                    :range int-literal
;;                    :bounded boolean}
;; identifier     := #{<string literal>}
;; int-literal    := #{<integer literal>}
;; string-literal := #{<string literal>}
;; wildcard       := '*
;; double-dot     := '..

#_{:clj-kondo/ignore [:unresolved-symbol]}
(defn- slice-list->slice-map [slice-list]
  ;; "start", "end", and "step" are singleton vectors as a result of w/postwalk
  (m/match [slice-list]
    [[":"]]
    {:start :vec-lower :end :vec-higher :step 1}
    [[":" ":"]]
    {:start :vec-lower :end :vec-higher :step 1}
    [[[start] ":"]]
    {:start start :end :vec-higher :step 1}
    [[":" [end]]]
    {:start :vec-lower :end end :step 1}
    [[[start] ":" [end]]]
    {:start start :end end :step 1}
    [[[start] ":" [end] ":"]]
    {:start start :end end :step 1}
    ;; Variable steps
    [[":" ":" [step]]]
    (if (pos-int? step)
      {:start :vec-lower :end :vec-higher :step step}
      {:start :vec-higher :end :vec-lower :step step})
    [[[start] ":" ":" [step]]]
    (if (pos-int? step)
      {:start start :end :vec-higher :step step}
      {:start start :end :vec-lower :step step})
    [[":" [end] ":" [step]]]
    (if (pos-int? step)
      {:start :vec-lower :end end :step step}
      {:start :vec-higher :end end :step step})
    [[[start] ":" [end] ":" [step]]]
    {:start start :end end :step step}
    :else
    (throw (ex-info "cannot process array slice" {:array-slice slice-list}))))

(defn- unquote-str [s]
  ;; Assume that quotes are symmetrical
  (cond
    ;; \"foo\" or \'foo\' 
    (or (= \' (first s)) (= \" (first s)))
    (subs s 1 (-> s count dec))
    ;; \\'foo\\'
    (and (= \\ (first s)) (= \' (second s)))
    (subs s 2 (-> s count dec dec))
    ;; foo
    :else
    s))

(defn- str->int
  [int-str]
  ;; Clojure uses Java longs, not ints
  (Long/parseLong int-str))

(defn- instaparse-node->pathetic
  [parsed]
  (if (coll? parsed)
    (case (first parsed)
      :jsonpaths  (->> parsed rest vec)
      :jsonpath   (->> parsed rest (apply concat) vec)
      :child      (->> parsed rest vec)
      :bracket-union (-> parsed rest flatten vec)
      ;; Child nodes
      :array-slice (slice-list->slice-map (apply vector (rest parsed)))
      :identifier  [(second parsed)]
      :string-literal [(-> parsed second unquote-str)]
      :int-literal [(-> parsed second str->int)]
      :wildcard '*
      :double-dot '..)
    parsed))

(defn instaparse->pathetic
  [parsed]
  (w/postwalk instaparse-node->pathetic parsed))

;; Mini-monad to deal with error threading
(defn- parse-bind
  [m f]
  (if-not (s/valid? ::parse-failure m) (f m) m))

(s/fdef parse
  :args (s/cat :path string?)
  :ret  (s/or :success ::json-paths
              :failure ::parse-failure))

(defn parse
  "Given a JSON-path, parse it into data. Returns a vector of parsed
   JSON-paths, or the first error map if one or more paths are
   invalid."
  [jsonpath-str]
  (let [parse-res (insta/parse jsonpath-instaparser jsonpath-str)]
    (parse-bind parse-res instaparse->pathetic)))

(s/fdef parse-first
  :args (s/cat :path string?)
  :ret  (s/or :success ::json-path
              :failure ::parse-failure))

(defn parse-first
  "Same as parse, but returns the first parsed JSON-path, or nil if the
   paths are invalid."
  [jsonpath-str]
  (let [parse-res (parse jsonpath-str)]
    (parse-bind parse-res first)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxillary/misc functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-parse-failure?
  "Returns true if the given object is error data from a parse
   failure, false otherwise."
  [x]
  (s/valid? ::parse-failure x))

(defn get-not-strict
  "Returns the first non-strict element in a parsed path, which 
   is any one of the following:
   - Recursive descent operator (\"..\")
   - Array slices
   - Negative array indices"
  [parsed-path]
  (letfn [(not-strict-element
            [elem]
           (cond (= '.. elem)
                 elem
                 (= '* elem)
                 nil
                 :else ;; vector
                 (some (fn [x] (when (or (map? x) (neg-int? x)) x))
                       elem)))]
    (some not-strict-element parsed-path)))

(defn path-element->string
  "Stringify a path element, e.g. [{:start 0 :end 5 :step 1}]
   becomes \"[0:5:1]\"."
  [element]
  (letfn [(sub-elem->str
            [elm]
            (if (map? elm)
              (str (:start elm) ":" (:end elm) ":" (:step elm))
              (str elm)))]
    (if (vector? element)
      ;; Keys/indices/slices
      (string/join "," (map sub-elem->str element))
      ;; ".." and "*"
      (str element))))

(defn path->string
  "Stringify a parsed path, e.g. ['* ['books']]
   becomes \"$['*']['books']\"."
  [parsed-path]
  (letfn [(elem->str [elm]
                     (if-not (= '.. elm)
                       (str "[" (path-element->string elm) "]")
                       (path-element->string elm)))]
    (str "$" (string/join (map elem->str parsed-path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path enumeration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; moved down from top lvl ns

(comment
  (s/fdef satisfied
    :args (s/cat :json-path ::json-path
                 :key-path :com.yetanalytics.pathetic.zip/key-path)
    :ret (s/nilable ::json-path))

  (defn satisfied
    "Given a json path and a key path, return nil if they diverge, or
   if they partially match return a seq of the covered pattern"
    [path key-path]
    (when-let [partial-sat
               (map first
                    (take-while
                     (fn [[p pk]] (cond
                                    (= p '*)  true
                                    (= p '..) true
                                    (set? p)  (contains? p pk)
                                    :else     (let [{:keys [start end step]} p]
                                                (some (partial = pk)
                                                      (range start end step)))))
                     (map vector
                          path
                          key-path)))]
      (cond
      ;; satisfied, we can keep it
        (= path partial-sat)
        path
      ;; otherwise we are partial, if there is more left in key path this is
      ;; is a failure
        (not-empty
         (drop (count partial-sat)
               key-path))
        nil
        :else partial-sat))))

;; found in datassim, may need to be elsewhere

(comment
  (s/def :enumerate/limit
    number?)

  (s/fdef enumerate
    :args (s/cat :path ::json-path
                 :options (s/keys* :opt-un [:enumerate/limit]))
    :ret (s/every ::json/key-path))

  (defn enumerate
    "Given a json path, return a lazy seq of concrete key paths.
   Wildcards/recursive/ranges will be enumerated up to :limit,
   which defaults to 10"
    [path & {:keys [limit] :or {limit 10}}]
    (map vec
         (apply combo/cartesian-product
                (map
                 (fn [element]
                   (cond
                     (set? element)
                     element
                     (= '* element)
                     (if limit (range limit) (range))
                   ;; otherwise, itsa range spec
                     :else
                     (let [{:keys [start end step]} element]
                       (cond->> (range start end step)
                         limit (take limit)))))
                 path)))))

(defn- normalize-indices
  "Normalize indices by doing the following:
   - Turn array splices into array index sequences
   - Turn negative array indices into positive ones"
  [keys json]
  (letfn [(clamp-start [len idx]
            (cond (< idx 0) 0
                  (> idx len) len
                  :else idx))
          (clamp-end [len idx]
            (cond (< idx -1) -1
                  (> idx len) len
                  :else idx))
          (norm-start [len start]
            (cond (= :vec-lower start) 0
                  (= :vec-higher start) (dec len)
                  (neg-int? start) (clamp-start len (+ len start))
                  :else (clamp-start len start)))
          (norm-end [len end]
            (cond (= :vec-lower end) -1
                  (= :vec-higher end) len
                  (neg-int? end) (clamp-end len (+ len end))
                  :else (clamp-end len end)))]
    (reduce (fn [acc elem]
              (cond
                (map? elem) ;; Element is an array splice
                (if (vector? json)
                  (let [{:keys [start end step]} elem
                        len   (count json)
                        start (norm-start len start)
                        end   (norm-end len end)
                        step  (if (zero? step) 1 step) ;; no infinite loops
                        nvals (range start end step)]
                    (vec (concat acc nvals)))
                  ;; JSON data is not a vector, return a dummy index such that
                  ;; (get json 0) => nil
                  (conj acc 0))
                (neg-int? elem)
                (conj acc (+ (count json) elem))
                :else
                (conj acc elem)))
            []
            keys)))

(defn- init-queue [init]
  (conj clojure.lang.PersistentQueue/EMPTY init))

;; Helper specs

(s/def ::fail boolean?)

(s/fdef path-seqs
  :args (s/cat :json ::json/json :path ::json-path)
  :ret (s/every (s/keys :req-un [::json/json ::json/path ::fail])
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
                               :rest (seq json-path)
                               :path []
                               :fail false
                               :desc false})]
    (if-not (every? (fn [{jsn :json rst :rest}] (or (nil? jsn) (empty? rst)))
                    worklist)
      (let [{jsn :json rst :rest pth :path dsc :desc :as workitem}
            (peek worklist)]
        (if-let [element (first rst)]
          (cond
            ;; Short circuit: if json is a primitive or nil stop traversal
            (not (coll? jsn))
            (if dsc
              ;; Scalar is the result of recursive descent, ignore
              (recur (pop worklist))
              ;; Scalar is more-or-less intentional, mark failure
              (let [worklist' (conj (pop worklist)
                                    {:json nil
                                     :rest rst
                                     :path pth
                                     :fail true
                                     :desc false})]
                (recur worklist')))
            ;; Recursive descent
            (= '.. element)
            (let [desc-list (json/recursive-descent jsn)
                  worklist' (reduce
                             (fn [acc desc]
                               (conj acc
                                     {:json (:json desc)
                                      :rest (rest rst)
                                      :path (vec (concat pth (:path desc)))
                                      :fail false
                                      :desc true}))
                             (pop worklist)
                             desc-list)]
              (recur worklist'))
            ;; Wildcard
            (= '* element)
            (let [worklist' (reduce-kv
                             (fn [acc key child]
                               (conj acc
                                     {:json child
                                      :rest (rest rst)
                                      :path (conj pth key)
                                      :fail false
                                      :desc false}))
                             (pop worklist)
                             jsn)]
              (recur worklist'))
            ;; Vector of keys/indices/slices
            (vector? element)
            (let [element'  (normalize-indices element jsn)
                  worklist' (reduce
                             (fn [acc sub-elm]
                               (if (and dsc (not (contains? jsn sub-elm)))
                                 acc ;; Recursive desc: don't add missing keys
                                 (conj acc
                                       {:json (get jsn sub-elm)
                                        :rest (rest rst)
                                        :path (conj pth sub-elm)
                                        :fail (not (contains? jsn sub-elm))
                                        :desc false})))
                             (pop worklist)
                             element')]
              (recur worklist')))
          ;; Path is exhausted; cycle work item to back of worklist
          (recur (conj (pop worklist) workitem))))
      (map #(dissoc % :rest :desc) worklist))))

(defn speculative-path-seqs
  "Similar to path-seqs, except it continues traversing the path even if
   the location in the JSON data is missing or incompatible. Returns the
   same fields as path-seqs except for :fail."
  [json-obj json-path]
  (loop [worklist (init-queue {:json json-obj
                               :rest (seq json-path)
                               :path []})]
    (if-not (every? (fn [{rst :rest}] (empty? rst)) worklist)
      (let [{jsn :json rst :rest pth :path :as workitem}
            (peek worklist)]
        (if-let [element (first rst)]
          (cond
            ;; Recursive descent: not allowed
            (= '.. element)
            (throw (ex-info "illegal path element" {}))
            ;; Wildcard: append to end
            (= '* element)
            (recur (conj (pop worklist)
                         {:json nil
                          :rest (rest rst)
                          :path (conj pth
                                      (cond (vector? jsn) (-> jsn count)
                                            (map? jsn) (-> jsn count str)
                                            :else 0))}))
            ;; Vector of keys/indices
            (vector? element)
            (if (or (some map? element) (some neg-int? element))
              (throw (ex-info "illegal path element" {}))
              (let [worklist' (reduce (fn [acc sub-elm]
                                        (conj acc
                                              {:json (get jsn sub-elm)
                                               :rest (rest rst)
                                               :path (conj pth sub-elm)}))
                                      (pop worklist)
                                      element)]
                (recur worklist'))))
          ;; Cycle workitem to end
          (recur (conj (pop worklist) workitem))))
      (seq worklist))))

(comment
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
                  [p hit])]
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
                     path))))