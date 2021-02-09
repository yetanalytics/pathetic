(ns com.yetanalytics.pathetic.json-path
  (:require [clojure.core.match :as m]
            [clojure.string :as string]
            [clojure.set :as cset]
            [clojure.walk :as w]
            [blancas.kern.core              :as k]
            [clojure.spec.alpha             :as s]
            [blancas.kern.lexer.basic       :as kl]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: JSON-conformant string literals
;; Integer and string literal regexes from:
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
    <child-body>  := wildcard | int-literal | identifier;
    
    identifier     := #'[a-zA-Z_]+[a-zA-Z_0-9]*';
    int-literal    := #'-?(0|[1-9][0-9]*)';
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
  ;; "start", "end", and "step" are singleton sets as a result of w/postwalk
  (m/match [slice-list]
    [[":"]]
    {:start 0 :end Long/MAX_VALUE :step 1 :bounded? false}
    [[":" ":"]]
    {:start 0 :end Long/MAX_VALUE :step 1 :bounded? false}
    [[":" end]]
    {:start 0 :end (first end) :step 1 :bounded? true}
    [[start ":"]]
    {:start (first start) :end Long/MAX_VALUE :step 1 :bounded? false}
    [[start ":" end]]
    {:start (first start) :end (first end) :step 1 :bounded? true}
    [[start ":" end ":"]]
    {:start (first start) :end (first end) :step 1 :bounded? true}
    [[start ":" end ":" step]]
    {:start (first start) :end (first end) :step (first step) :bounded? true}))

;; Regex from:
;; https://stackoverflow.com/questions/56618450/how-to-remove-matching-quotes-when-quotes-surrounds-word-that-starts-with-or
(defn- unquote-str
  [quoted-str]
  (-> quoted-str
      (string/replace #"\"([^\"]*)\"" "$1")
      (string/replace #"\'([^\']*)\'" "$1")))

(defn- str->int
  [int-str]
  (Integer/parseInt int-str))

(defn- instaparse-node->pathetic
  [parsed]
  (if (coll? parsed)
    (case (first parsed)
      :jsonpaths  (-> parsed rest vec)
      :jsonpath   (-> parsed rest flatten vec)
      :child      (-> parsed rest vec)
      :bracket-union (reduce (fn [acc elem] (if (set? elem)
                                              (cset/union acc elem)
                                              (conj acc elem)))
                             #{}
                             (rest parsed))
      ;; Child nodes
      :array-slice (slice-list->slice-map (apply vector (rest parsed)))
      :identifier  #{(second parsed)}
      :string-literal #{(-> parsed second unquote-str)}
      :int-literal #{(-> parsed second str->int)}
      :wildcard '*
      :double-dot '..)
    parsed))

(defn instaparse->pathetic
  [parsed]
  (w/postwalk instaparse-node->pathetic parsed))

(instaparse->pathetic (first (insta/parses jsonpath-instaparser "$.store.book|$.results.extensions")))
#_(instaparse->pathetic (first (insta/parses jsonpath-instaparser "$..*.authors")))

(s/fdef parse
  :args (s/cat :path string?)
  :ret  (s/nilable ::json-paths))

(defn parse
  "Given a JSON-path, parse it into data. Returns a vector of parsed
   JSON-paths, or nil if one or more paths are invalid."
  [jsonpath-str]
  (let [paths (->> jsonpath-str
                   (insta/parses jsonpath-instaparser)
                   first
                   instaparse->pathetic)]
    (if (not-empty (filterv empty? paths))
      nil
      paths)))

(s/fdef parse-first
  :args (s/cat :path string?)
  :ret  (s/nilable ::json-path))

(defn parse-first
  "Same as parse, but returns the first parsed JSON-path, or nil if the
   paths are invalid."
  [jsonpath-str]
  (-> jsonpath-str parse first))

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
                   path)))