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
     (if-let [empty-coll (empty node)]
       (into empty-coll
             kids)
       ;; if clojure.core/empty doesn't work, check for map entry
       (if (map-entry? node)
         (if (= 2 (count kids))
           (let [[k v] kids]
             (clojure.lang.MapEntry. k v))
           (throw (ex-info "Can only have two children in a MapEntry"
                           {:type ::map-entry-constraint
                            :node node
                            :children kids})))
         (throw (ex-info (format "Don't know how to make %s node" (type node))
                         {:type ::unknown-collection
                          :node node
                          :node-type (type node)
                          :children kids})))))
   root))

(s/fdef internal?
  :args (s/cat :loc ::loc)
  :ret boolean?)

(defn internal?
  "Is a location internal, ie a map entry or key"
  [loc]
  (let [node (z/node loc)]
    (or (map-entry? node)
        ;; key position
        (and (string? node)
             (zero? (count (z/lefts loc)))
             (some-> loc z/up z/node map-entry?))
        false)))

(s/def ::key
  (s/or :index (s/int-in 0 Integer/MAX_VALUE)
        :key (s/or :string string?
                   :keyword keyword?)))

(s/fdef el-key
  :args (s/cat :loc ::loc)
  :ret (s/nilable
        ::key))

(defn el-key
  [loc]
  (when-not (internal? loc)
    (when-let [p (peek (z/path loc))]
      (cond
        (map-entry? p)
        (key p)

        (vector? p)
        (count (z/lefts loc))))))

(s/def ::key-path
  (s/every ::key))

(s/fdef k-path
  :args (s/cat :loc ::loc)
  :ret (s/nilable
        ::key-path))

(defn k-path
  [loc]
  (into []
        (reverse
         (keep el-key
               (take-while some?
                           (iterate z/up loc))))))

;; given a root and a key-path, can we return a loc at that path?
;; this would make up some for the inefficiency of having to walk everything
;; when there is a known path?

(s/fdef get-child
  :args (s/cat :loc (s/nilable ::loc)
               :k ::key)
  :ret (s/nilable ::loc))

(defn get-child
  "Returns the child of loc at k or nil if key not present.
  Will skip map-entries entirely, like clojure.core/get"
  [loc k]
  (when loc
    (if (z/branch? loc)
      (if (internal? loc)
        (throw (ex-info "called get-child on an internal node (map entry or key)"
                        {:type ::get-child-on-internal
                         :loc loc
                         :key k}))
        (when-let [[fk fv :as found] (find node k)]
          (let [child-locs (iterate z/right
                                    (z/down loc))]
            (if (map? (z/node loc))
              ;; if the node is a map, we want to skip the map entries
              (-> (some
                   (fn [cl]
                     (when (= found (z/node cl))
                       cl))
                   child-locs)
                  z/down
                  z/right)
              (nth child-locs fk)))))
      (throw (ex-info "called get-child on a leaf node"
                      {:type ::get-child-on-leaf
                       :loc loc
                       :key k})))))

(s/fdef get-child-in
  :args (s/cat :loc (s/nilable ::loc)
               :key-path ::key-path)
  :ret (s/nilable ::loc))


(defn get-child-in
  "Like clojure.core/get-in, but for zipper structures."
  [loc key-path]
  (reduce get-child loc key-path))

(s/fdef loc-in
  :args (s/cat :root ::any-json
               :key-path ::key-path)
  :ret (s/nilable ::loc))

(defn loc-in
  "Convenience, like get-child-in, but it takes root and returns a loc or nil."
  [root key-path]
  (-> root json-zip (get-child-in key-path)))

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
