(ns com.yetanalytics.pathetic.json
  "Specs and util functions for JSON value representations."
  (:require [clojure.spec.alpha :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spec representing a generic JSON object.

(s/def ::json
  (s/or :coll
        (s/or :map (s/map-of string? ::json :gen-max 5)
              :vector (s/coll-of ::json :kind vector? :gen-max 5))
        :scalar
        (s/or :null nil?
              :string string?
              :boolean boolean?
              :number (s/or :double (s/double-in :infinite? false :NaN? false)
                            :int int?))))

;; Duplicate spec body to avoid forward definition
(s/def ::coll
  (s/or :map (s/map-of string? ::json :gen-max 5)
        :vector (s/coll-of ::json :kind vector? :gen-max 5)))

;; Key paths, a subset of what clojure uses for get/assoc/update-in that
;; applies to JSON.
;; Unlike parsed JSONPath vectors, these represent definite paths (so no
;; recursive descent, wildcards, unions, or array splicing).

(s/def ::key (s/or :index int? :key string?))

(s/def ::path (s/every ::key :kind vector?))

(s/def ::paths (s/every ::path :kind vector? :min-count 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef recursive-descent
  :args (s/cat :json ::json)
  :ret  (s/every (s/keys :req-un [::json ::path]) :kind vector?))

(defn- recursive-descent*
  [json path children]
  (let [children (conj children {:json json :path path})]
    (cond
      (coll? json)
      (reduce-kv (fn [acc k v]
                   (recursive-descent* v (conj path k) acc))
                 children
                 json)
      :else
      children)))

(defn recursive-descent
  "Perform the recursive descent operation (\"..\" in JSONPath syntax).
   Returns all possible sub-structures of a JSON data structure."
  [json] (recursive-descent* json [] []))

(s/fdef jassoc
  :args (s/cat :coll (s/nilable ::coll)
               :k (s/or :index (s/with-gen int? #(s/gen (s/int-in 0 100)))
                        :key string?)
               :v ::json)
  :ret  ::coll)

(defn- pad-vector-nils
  [coll k v]
  (loop [idx   (count coll)
         coll' (transient coll)]
    (if (= idx k)
      (persistent! (assoc! coll' k v))
      (recur (inc idx) (assoc! coll' idx nil)))))

(defn jassoc
  "Like `assoc`, but with the following differences:
   - Automatically dispatches on the type of k for colls.
     If k is a string, we assoc the key-val pair to a map,
     otherwise we assoc it to a vector.
   - If coll is a scalar or is the wrong coll, (e.g. a map
     with a vector key), we overwrite the original coll.
   - If the vector index is out of bounds, we increase the size
     of the vector, filling in skipped entries with nils."
  [coll k v]
  (cond
    ;; String key - maps
    (string? k)
    (if (map? coll)
      (assoc coll k v)
      (assoc {} k v))
    ;; Integer key - vectors
    (int? k)
    (let [coll (if (vector? coll) coll [])]
      (if (< k (count coll))
        (assoc coll k v)
        (pad-vector-nils coll k v)))))

(s/fdef jassoc-in
  :args (s/cat :m (s/nilable ::coll)
               :ks (s/every (s/or :index (s/int-in 0 1000) :key string?))
               :v ::json)
  :ret  ::json)

(defn jassoc-in
  "Like `assoc-in`, but for `jassoc`. Returns `v` if the keys seq is
   empty."
  [m [k & ks] v]
  (cond
    (not k) v
    ks      (jassoc m k (jassoc-in (get m k) ks v))
    :else   (jassoc m k v)))
