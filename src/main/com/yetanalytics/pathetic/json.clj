(ns com.yetanalytics.pathetic.json
  (:require [clojure.spec.alpha :as s]))

;; Spec representing a generic JSON object.

(s/def ::any
  (s/or :scalar (s/or :null nil?
                      :string string?
                      :boolean boolean?
                      :number (s/or :double (s/double-in :infinite? false
                                                         :NaN? false)
                                    :int int?))
        :coll (s/or :map (s/map-of string? ::any)
                    :vector (s/coll-of ::any :kind vector?))))

(s/def ::json (s/nilable ::any))

;; Key paths, a subset of what clojure uses for get/assoc/update-in that
;; applies to JSON.
;; Unlike parsed JSONPath vectors, these represent definite paths (so no
;; recursive descent, wildcards, unions, or array splicing).

(s/def ::key
  (s/or :index (s/int-in 0 Integer/MAX_VALUE) ; Negative indices get normalized
        :key (s/or :string string?
                   :keyword keyword?)))

(s/def ::path (s/every ::key
                       :type vector?))

(s/def ::paths (s/every ::key-path
                        :type vector?
                        :min-count 1))

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
