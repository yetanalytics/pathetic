(ns com.yetanalytics.pathetic.parse
  "JSONPath parsing, alongside parse validation and parsed path -> string
   functions."
  (:require #?(:clj [clojure.core.match :as m]
               :cljs [cljs.core.match :as m])
            [clojure.spec.alpha :as s]
            [clojure.string     :as cstr]
            [clojure.walk       :as w]
            [instaparse.core    :as insta]
            [com.yetanalytics.pathetic.path :as path]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (if (nat-int? step)
      {:start :vec-lower :end :vec-higher :step step}
      {:start :vec-higher :end :vec-lower :step step})
    [[[start] ":" ":" [step]]]
    (if (nat-int? step)
      {:start start :end :vec-higher :step step}
      {:start start :end :vec-lower :step step})
    [[":" [end] ":" [step]]]
    (if (nat-int? step)
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
  :ret  (s/or :success ::path/paths
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
  :ret  (s/or :success ::path/path
              :failure ::parse-failure))

(defn parse-first
  "Same as `parse`, but returns the first parsed JSON-path, or `nil`
   if the paths are invalid."
  [jsonpath-str]
  (let [parse-res (parse jsonpath-str)]
    (parse-bind parse-res first)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef is-parse-failure?
  :args (s/cat :x (s/or :success ::path/paths
                        :failure ::parse-failure))
  :ret boolean?)

(defn is-parse-failure?
  "Returns true if the given object is error data from a parse
   failure, false otherwise."
  [x]
  (s/valid? ::parse-failure x))

(s/fdef test-strict-path
  :args (s/cat :parsed-path ::path/path)
  :ret (s/nilable ::path/element))

(defn test-strict-path
  "Test if a parsed path is valid in strict mode. If so, returns
   nil; if not, then returns the first non-strict element, which 
   is any one of the following:
   - Recursive descent operator (\"..\")
   - Array slicess
   - Negative array indices"
  [parsed-path]
  (->> parsed-path
       (filter (fn [e] (not (s/valid? ::path/strict-element e))))
       first))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef path->string
  :args (s/cat :parsed-path ::path/path)
  :ret  string?
  :fn   (fn [{:keys [args ret]}] (= (:parsed-path args) (parse-first ret))))

(defn path->string
  "Stringify a parsed path back into a JSONPath string."
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
              :else (str "[" (cstr/join "," (map sub-elem->str elm)) "]")))]
    (str "$" (cstr/join (map elem->str parsed-path)))))
