(ns com.yetanalytics.pathetic.json-path
  (:require #?(:clj [clojure.core.match :as m]
               :cljs [cljs.core.match :as m])
            [clojure.string     :as string]
            [clojure.walk       :as w]
            [clojure.spec.alpha :as s]
            [instaparse.core    :as insta]
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

(defn valid-recursive-descent?
  "Returns false if the path vector does not have a '.. symbol followed
   by no children or another '.., true otherwise."
  [path]
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

;; Indefinite paths

(s/def ::wildcard #{'*})
(s/def ::recursive #{'..})

(s/def :slice/start (s/or :index int? :limit #{:vec-higher :vec-lower}))
(s/def :slice/end (s/or :index int? :limit #{:vec-higher :vec-lower}))
(s/def :slice/step int?)
(s/def ::slice (s/keys :req-un [:slice/start
                                :slice/end
                                :slice/step]))

(s/def ::keyset
  (s/every (s/or :key string? :index int? :slice ::slice)
           :type vector?
           :min-count 1
           :gen-max 5))

(s/def ::element
  (s/or :keyset ::keyset
        :wildcard ::wildcard
        :recursive ::recursive))

(s/def ::json-path
  (s/and (s/every ::element
                  :type vector?
                  :min-count 1)
         valid-recursive-descent?))

(s/def ::json-paths
  (s/every ::json-path
           :type vector?
           :min-count 1))

;; Strict versions of above

(s/def ::strict-keyset
  (s/every (s/or :key string? :index nat-int?)
           :type vector?
           :min-count 1
           :gen-max 5))

(s/def ::strict-element
  (s/or :keyset   ::strict-keyset
        :wildcard ::wildcard))

(s/def ::strict-json-path
  (s/every ::strict-element
           :type vector?
           :min-count 1))

(s/def ::strict-json-paths
  (s/every ::strict-json-paths
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
   "jsonpaths  := <ws> jsonpath (<ws> <'|'> <ws> jsonpath)* <ws>;
    jsonpath   := <root> children?;
    <children> := child+;
    child      := bracket-child | dot-child;

    <bracket-child>   := double-dot? <'['> <ws> bracket-content <ws> <']'>;
    <bracket-content> := wildcard | bracket-union;
    bracket-union     := union-element (<ws> <','> <ws> union-element)*;
    <union-element>   := int-literal | string-literal | array-slice;
    array-slice       := int-literal? ':' int-literal? (':' int-literal?)?;
  
    <dot-child>   := double-dot child-body | <dot> child-body;
    <child-body>  := wildcard | identifier;
    
    identifier     := #'[a-zA-Z0-9_\\-]+';
    int-literal    := #'-?[0-9]+';
    string-literal := string-literal-sq | string-literal-dq
    <string-literal-sq> := #'\\'(?:\\\\[\\'bfnrt/\\\\]|\\\\u[a-fA-F0-9]{4}|[^\\'\\\\])*\\'';
    <string-literal-dq> := #'\"(?:\\\\[\"bfnrt/\\\\]|\\\\u[a-fA-F0-9]{4}|[^\"\\\\])*\"'

    root       := '$';
    wildcard   := '*';
    double-dot := '..';
    dot        := '.';
    ws         := #'\\s*'
  "))

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
    (throw (ex-info "Cannot process array slice"
                    {:type ::invalid-array-slice
                     :array-slice slice-list}))))

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
  #?(:clj  (Long/parseLong int-str)
     :cljs (js/parseInt int-str)))

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

(s/fdef is-parse-failure?
  :args (s/cat :x (s/or :success ::json-paths :failure ::parse-failure))
  :ret boolean?)

(defn is-parse-failure?
  "Returns true if the given object is error data from a parse
   failure, false otherwise."
  [x]
  (s/valid? ::parse-failure x))

(s/fdef test-strict-path
  :args (s/cat :parsed-path ::json-path)
  :ret (s/nilable ::element))

(defn test-strict-path
  "Test if a parsed path is valid in strict mode. If so, returns
   nil; if not, then returns the first non-strict element, which 
   is any one of the following:
   - Recursive descent operator (\"..\")
   - Array slices
   - Negative array indices"
  [parsed-path]
  (->> parsed-path
       (filter (fn [e] (not (s/valid? ::strict-element e))))
       first))

(s/fdef path->string
  :args (s/cat :parsed-path ::json-path)
  :ret string?)

(defn path->string
  "Stringify a parsed path, e.g. ['* ['books']]
   becomes \"$['*']['books']\"."
  [parsed-path]
  (letfn [(sub-elem->str
            [sub-elm]
            (cond
              (map? sub-elm)
              (let [{:keys [start end step]} sub-elm
                    start (if (keyword? start) nil start)
                    end   (if (keyword? end) nil end)]
                (str start ":" end ":" step))
              (string? sub-elm)
              (str "'" sub-elm "'")
              :else
              (str sub-elm)))
          (elem->str
            [elm]
            (cond
              (= '* elm) "[*]"
              (= '.. elm) ".."
              :else (str "[" (string/join "," (map sub-elem->str elm)) "]")))]
    (str "$" (string/join (map elem->str parsed-path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path enumeration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helper functions

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
    (persistent!
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
                     (reduce (fn [acc v] (conj! acc v)) acc nvals))
                   ;; JSON data is not a vector, return a dummy index such that
                   ;; (get json 0) => nil
                   (conj! acc 0))
                 (neg-int? elem)
                 (conj! acc (+ (count json) elem))
                 :else
                 (conj! acc elem)))
             (transient [])
             keys))))

(defn- init-queue [init]
  #?(:clj (conj clojure.lang.PersistentQueue/EMPTY init)
     :cljs (conj cljs.core/PersistentQueue.EMPTY init)))

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
                               :rest json-path
                               :path []
                               :desc false})
         reslist  (transient [])]
    (if-let [{jsn :json rst :rest pth :path dsc :desc} (peek worklist)]
      (if-let [element (first rst)]
        ;; We order the conditions heuristically based on likelihood of
        ;; encounter, in order to minimize equiv operations
        (cond
          ;; Short circuit: if json is a primitive or nil stop traversal
          (not (coll? jsn))
          (if dsc
            (recur (pop worklist) reslist) ;; Recursive desc; ignore
            (let [worklist' (pop worklist) ;; Otherwise, mark failure
                  reslist'  (conj! reslist
                                   {:json nil
                                    :path pth
                                    :fail true})]
              (recur worklist' reslist')))
          ;; Vector of keys/indices/slices
          (vector? element)
          (let [element'
                (normalize-indices element jsn)
                [worklist' reslist']
                (reduce
                 (fn [[worklist reslist] sub-elm]
                   ;; Need to separate `contains?` from `get` to distinguish
                   ;; nil values from non-existent ones in the coll.
                   (if (contains? jsn sub-elm)
                     [(conj worklist
                            {:json (get jsn sub-elm)
                             :rest (rest rst)
                             :path (conj pth sub-elm)
                             :desc false})
                      reslist]
                     (if dsc
                       [worklist reslist] ;; Recursive desc: don't add missing keys
                       [worklist          ;; Otherwise, mark failure
                        (conj! reslist
                               {:json nil
                                :path (conj pth sub-elm)
                                :fail true})])))
                 [(pop worklist) reslist]
                 element')]
            (recur worklist' reslist'))
          ;; Wildcard
          (= '* element)
          (let [worklist' (reduce-kv
                           (fn [worklist key child]
                             (conj worklist
                                   {:json child
                                    :rest (rest rst)
                                    :path (conj pth key)
                                    :desc false}))
                           (pop worklist)
                           jsn)]
            (recur worklist' reslist))
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
          :else
          (throw (ex-info "Illegal path element"
                          {:type    ::illegal-path-element
                           :strict? false
                           :element element})))
        ;; Path is exhausted; move item from worklist to reslist
        (let [worklist' (pop worklist)
              reslist'  (conj! reslist
                               {:json jsn
                                :path pth
                                :fail false})]
          (recur worklist' reslist')))
      (persistent! reslist))))

(defn speculative-path-seqs
  "Similar to path-seqs, except it continues traversing the path even if
   the location in the JSON data is missing or incompatible. Returns the
   same fields as path-seqs except for :fail."
  [json-obj json-path]
  (loop [worklist (init-queue {:json json-obj
                               :rest json-path
                               :path []})
         reslist  (transient [])]
    (if-let [{jsn :json rst :rest pth :path} (peek worklist)]
      (if-let [element (first rst)]
        (cond
          ;; Vector of keys/indices
          (vector? element)
          (let [worklist'
                (reduce (fn [worklist sub-elm]
                          (if (or (string? sub-elm) (nat-int? sub-elm))
                            (conj worklist
                                  {:json (get jsn sub-elm)
                                   :rest (rest rst)
                                   :path (conj pth sub-elm)})
                            (throw (ex-info "Illegal path element"
                                            {:type    ::illegal-path-element
                                             :strict? true
                                             :element element}))))
                        (pop worklist)
                        element)]
            (recur worklist' reslist))
          ;; Wildcard: append to end
          (= '* element)
          (let [worklist'
                (conj (pop worklist)
                      {:json nil
                       :rest (rest rst)
                       :path (conj pth (cond (vector? jsn) (-> jsn count)
                                             (map? jsn) (-> jsn count str)
                                             :else 0))})]
            (recur worklist' reslist))
          :else ;; Includes recursive descent
          (throw (ex-info "Illegal path element"
                          {:type    ::illegal-path-element
                           :strict? true
                           :element element})))
        ;; Move workitem from worklist to reslist
        (let [worklist' (pop worklist)
              reslist'  (conj! reslist
                               {:json jsn
                                :path pth})]
          (recur worklist' reslist')))
      (persistent! reslist))))
