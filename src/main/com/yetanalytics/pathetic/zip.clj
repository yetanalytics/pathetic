(ns com.yetanalytics.pathetic.zip
  (:require [clojure.zip :as z]
            [clojure.spec.alpha :as s]))

(s/def ::any-json
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
                ::any-json
                :gen-max 4)
               :vector
               (s/coll-of
                ::any-json
                :kind vector?
                :into []
                :gen-max 4)))))

(s/def :com.yetanalytics.pathetic.zip.loc.ppath/l
  (s/nilable
   (s/every any?)))

(s/def :com.yetanalytics.pathetic.zip.loc.ppath/r
  (s/nilable
   (s/every any?)))

(s/def :com.yetanalytics.pathetic.zip.loc.ppath/pnodes
  (s/nilable
   (s/every any?)))

(s/def :com.yetanalytics.pathetic.zip.loc/ppath
  (s/nilable
   (s/keys :req-un
           [:com.yetanalytics.pathetic.zip.loc.ppath/l
            :com.yetanalytics.pathetic.zip.loc.ppath/r
            :com.yetanalytics.pathetic.zip.loc.ppath/pnodes
            :com.yetanalytics.pathetic.zip.loc/ppath])))

(s/def ::loc
  (s/tuple any?
           :com.yetanalytics.pathetic.zip.loc/ppath))


(s/fdef json-zip
  :args (s/cat :root ::any-json)
  :ret ::loc
  :fn (fn [{{root :root} :args
            [node _] :ret}]
        (= root node)))

(defn json-zip
  "Produce a zipper for the JSON"
  [root]
  (z/zipper
   coll?
   seq
   (fn make-node
     [node kids]
     (if (map-entry? node)
       (if (= 2 (count kids))
         (let [[k v] kids]
           (clojure.lang.MapEntry. k v))
         (throw (ex-info "Can only have two children in a MapEntry"
                         {:type ::map-entry-constraint
                          :node node
                          :children kids})))
       (into (empty node)
             kids)))
   root))

(s/fdef internal?
  :args (s/cat :loc ::loc)
  :ret boolean?)

(defn internal?
  "Is a location internal, ie a map entry or key"
  [[node {:keys [l r ppath pnodes]} :as loc]]
  (or (map-entry? node)
      ;; key position
      (and (some-> loc z/up z/node map-entry?)
           (some-> l count (= 0)))
      false))

(s/def ::key
  (s/or :index pos-int?
        :key (s/or :string string?
                   :keyword keyword?)))

(s/fdef el-key
  :args (s/cat :loc ::loc)
  :ret (s/nilable
        ::key))

(defn el-key
  [[node {:keys [l r ppath pnodes]} :as loc]]
  (when-not (internal? loc)
    (let [p (peek pnodes)]
      (cond
        (map-entry? p)
        (key p)

        (vector? p)
        (count l)))))

(s/def ::key-path
  (s/every ::key))

(s/fdef k-path
  :args (s/cat :loc ::loc)
  :ret (s/nilable
        ::key-path))

(defn k-path
  [[node {:keys [l r ppath pnodes]} :as loc]]
  (into []
        (reverse
         (keep el-key (take-while some?
                                  (iterate z/up loc))))))

(s/def ::path-map
  (s/map-of
   ::key-path
   any?))


(s/fdef json-locs
  :args (s/cat :json ::any-json)
  :ret (s/every ::loc)
  :fn (fn [{locs :ret}]
        (every? (complement internal?) locs)))

(defn json-locs
  [json]
  (->> json
       json-zip
       (iterate z/next)
       (take-while (complement z/end?))
       ;; don't look at map entries/keys
       (remove internal?)))

(s/fdef json->path-map
  :args (s/cat :json ::any-json)
  :ret ::path-map)

(defn json->path-map
  "given some json, return a map of full paths to values"
  [json]
  (into {}
        (map (fn [loc]
               [(k-path loc) (z/node loc)])
             (json-locs json))))

(s/fdef path-map->json
  :args (s/cat :path-map ::path-map)
  :ret ::any-json)

(defn path-map->json
  [path-map]
  (get path-map []))
