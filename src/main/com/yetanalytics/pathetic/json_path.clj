(ns com.yetanalytics.pathetic.json-path
  (:require [blancas.kern.core :as k]
            [blancas.kern.lexer.basic :as kl]
            [clojure.spec.alpha :as s]))

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



(defrecord RangeSpec [start end step])

(s/def ::range
  (s/keys :req-un [:range/start
                   :range/end
                   :range/step]))

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
                          (Long/parseLong step))))))

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
     (k/<+> (k/many1 k/alpha-num)))
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
