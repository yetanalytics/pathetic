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

(s/def ::root #{'$})

(s/def ::wildcard #{'*})

(s/def ::recursive #{'..})

(s/def :range/start
  int?)

(s/def :range/end
  int?)

(s/def :range/step
  int?)

(s/def :range/bounded? ;; was this range bounded, or does it use a MAX_VALUE?
  boolean?)

(comment
  (defrecord RangeSpec [start end step bounded?])

  (s/fdef range-spec?
    :args (s/cat :item any?)
    :ret boolean?)

  (defn range-spec?
    [item]
    (instance? RangeSpec item)))

(s/def ::range
  (s/keys :req-un [:range/start
                   :range/end
                   :range/step
                   :range/bounded?]))

(s/def ::keyset
  (s/every (s/or :key string? :index int? :range ::range)
           :type set?
           :into #{}
           :min-count 1))

(comment
  (def ^:const max-long-str
    (str Long/MAX_VALUE)))

(comment
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
           path))))

(s/def ::json-path
  (s/every (s/or :keyset    ::keyset
                 :wildcard  ::wildcard
                 :recursive ::recursive)))

(s/def ::json-paths (s/every ::json-path))

;; Instaparse parsing failures

(s/def ::tag keyword?)
(s/def ::expecting any?) ;; Just need that keyword to exist
(s/def ::reason (s/coll-of (s/keys :req-un [::tag ::expecting])))
(s/def ::index int?)

(s/def ::parse-failure (s/keys :req-un [::index ::reason]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: JSON-conformant string literals
;; Integer and string literal regexes inspired from:
;; https://github.com/dchester/jsonpath/blob/master/lib/dict.js
(def jsonpath-instaparser
  (insta/parser
   "jsonpaths  := jsonpath (<#'\\s'>* <'|'> <#'\\s'>* jsonpath)*;
    jsonpath   := <root> children?;
    <children> := child+;
    child      := bracket-child | dot-child;

    <bracket-child>   := double-dot? <'['> <#'\\s'*> bracket-content <#'\\s'*> <']'>;
    <bracket-content> := wildcard | bracket-union;
    bracket-union     := union-element (<#'\\s'>* <','> <#'\\s'>* union-element)*;
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

;; Regex from:
;; https://stackoverflow.com/questions/56618450/how-to-remove-matching-quotes-when-quotes-surrounds-word-that-starts-with-or
(defn- unquote-str
  [quoted-str]
  (-> quoted-str
      (string/replace #"\"([^\"]*)\"" "$1")
      (string/replace #"\'([^\']*)\'" "$1")))

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

(instaparse->pathetic (insta/parse jsonpath-instaparser "$[0:2].key"))

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
;; Library functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; moved down from top lvl ns

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
      :else partial-sat)))

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

(defn- recursive-descent
  "Perform the recursive descent operation (\"..\" in JSONPath syntax).
   Returns all possible sub-structures of a JSON data structure."
  ([json] (recursive-descent json [] []))
  ([json path children]
   (let [children (conj children {:json json :path path})]
     (cond
       (coll? json)
       (reduce-kv (fn [acc k v] (recursive-descent v (conj path k) acc))
                  children
                  json)
       :else
       children))))

(defn- splice->indices
  "Turn an array splice into a series of array indices."
  [keys json]
  (reduce (fn [acc elem]
            (if (map? elem)
              ;; Element is an array splice
              (do
                (assert (coll? json) "splice cannot operate on JSON primitive")
                (let [{:keys [start end step]} elem
                      len   (count json)
                      start (cond (= :vec-lower start) 0
                                  (= :vec-higher start) (dec len)
                                  (neg-int? start) (+ len start)
                                  :else start)
                      end   (cond (= :vec-lower end) -1
                                  (= :vec-higher end) len
                                  (neg-int? end) (+ len end)
                                  :else end)
                      step  (if (zero? step) 1 step) ;; no infinite loops
                      nvals (range start end step)]
                  (vec (concat acc nvals))))
              (conj acc elem)))
          []
          keys))

(defn- init-queue [init]
  (conj clojure.lang.PersistentQueue/EMPTY init))

;; enumerate = enum-vec
;; get-at = values-vec
;; select-keys-at = (select-keys json enum-vec)
;; excise = (dissoc-in enum-vec json)
;; apply-values = (update-in json enum-vec fn)
(defn path-seqs
  "Given a JSON object and a parsed JSONPath, return a seq of
   maps with the following fields:
     :json  the JSON value at the JSONPath location.
     :path  the concrete JSONPath that was traversed.
     :rest  the remaining JSONPath that could not be traversed
     :fail  if the JSONPath traversal failed due to missing keys/indices
     :desc  if the previous JSONPath element was the recursive descent op"
  [json-obj json-path]
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
            (let [desc-list (recursive-descent jsn)
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
            (let [element'  (splice->indices element jsn)
                  worklist' (reduce
                             (fn [acc sub-elm]
                               (conj acc
                                     {:json (get jsn sub-elm)
                                      :rest (rest rst)
                                      :path (conj pth sub-elm)
                                      :fail (not (contains? jsn sub-elm))
                                      :desc false}))
                             (pop worklist)
                             element')]
              (recur worklist')))
          ;; Path is exhausted; cycle work item to back of worklist
          (recur (conj (pop worklist) workitem))))
      (map #(dissoc % :path :desc) worklist))))

(path-seqs {"universe" [{"foo" {"bar" 1}} {"baz" 2}]}
           ['* #{0 1} #{"foo"} #{"bar"}])

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