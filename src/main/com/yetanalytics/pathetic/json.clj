(ns com.yetanalytics.pathetic.json
  (:require [clojure.spec.alpha :as s]))

(s/def ::any
  (s/nilable
   (s/or :scalar
         (s/or :string
               string?
               :number
               (s/or :double
                     (s/double-in :infinite? false :NaN? false
                                  :max 1000.0 :min -1000.0)
                     :int
                     int?)
               :boolean
               boolean?)
         :coll
         (s/or :map
               (s/map-of
                string?
                ::any
                :gen-max 4)
               :vector
               (s/coll-of
                ::any
                :kind vector?
                :into []
                :gen-max 4)))))

;; Key paths, a subset of what clojure uses for get/assoc/update-in that applies
;; to JSON

(s/def ::key
  (s/or :index (s/int-in 0 Integer/MAX_VALUE)
        :key (s/or :string string?
                   :keyword keyword?)))

(s/def ::key-path
  (s/every ::key))

(defn jassoc
  "Like assoc, but if the first arg is nil it will dispatch on key to create the
   value, placing val in a new vector if needed. Only has the one simple arity"
  [coll k v]
  (if (or coll (string? k))
    (assoc coll k v)
    (assoc [] k v)))

(defn jassoc-in
  "like assoc-in, but for jassoc"
  [m [k & ks] v]
  (if ks
    (jassoc m k (jassoc-in (get m k) ks v))
    (jassoc m k v)))

