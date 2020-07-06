(ns com.yetanalytics.pathetic.zip
  (:require [clojure.zip :as z]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [com.yetanalytics.pathetic.json :as json]))

(s/def :com.yetanalytics.pathetic.zip.loc.ppath/l
  (s/nilable
   (s/every ::json/any)))

(s/def :com.yetanalytics.pathetic.zip.loc.ppath/r
  (s/nilable
   (s/every ::json/any)))

(s/def :com.yetanalytics.pathetic.zip.loc.ppath/pnodes
  (s/nilable
   (s/every ::json/any)))

(s/def :com.yetanalytics.pathetic.zip.loc/ppath
  (s/nilable
   (s/keys :req-un
           [:com.yetanalytics.pathetic.zip.loc.ppath/l
            :com.yetanalytics.pathetic.zip.loc.ppath/r
            :com.yetanalytics.pathetic.zip.loc.ppath/pnodes
            :com.yetanalytics.pathetic.zip.loc/ppath])))

(declare json-zip)

(s/def ::loc
  (s/with-gen (s/tuple ::json/any
                       :com.yetanalytics.pathetic.zip.loc/ppath)
    (fn []
      (sgen/bind
       (s/gen ::json/any)
       (fn [any-json]
         (sgen/elements
          (take-while (complement z/end?)
                      (iterate z/next
                               (json-zip any-json)))))))))


(s/fdef json-zip
  :args (s/cat :root ::json/any)
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

(s/fdef prune
  :args (s/cat :loc ::loc)
  :ret ::loc)

(defn prune
  "Remove the current node, if it is a value in a map entry also remove the parent.
   Shouldn't get called on root"
  [loc]
  (let [ploc (z/up loc)
        pnode (z/node ploc)]
    (z/remove
     (if (map-entry? pnode)
       ploc
       loc))))

;; given a root and a key-path, can we return a loc at that path?
;; this would make up some for the inefficiency of having to walk everything
;; when there is a known path?

(s/fdef get-child
  :args (s/cat :loc ::loc
               :k ::key)
  :ret (s/nilable ::loc))

(defn get-child
  "Returns the child of loc at k or nil if key not present.
  Will skip map-entries entirely, like clojure.core/get"
  [loc k]
  (when (and loc
             (z/branch? loc)
             (not (internal? loc)))
    (let [node (z/node loc)]
      (when-let [[fk fv :as found] (find node k)]
        (let [child-locs (iterate z/right
                                  (z/down loc))]
          (if (map? node)
            ;; if the node is a map, we want to skip the map entries
            (-> (some
                 (fn [cl]
                   (when (= found (z/node cl))
                     cl))
                 child-locs)
                z/down
                z/right)
            (nth child-locs fk)))))))

(s/fdef get-child-in
  :args (s/cat :loc (s/nilable ::loc)
               :key-path ::key-path)
  :ret (s/nilable ::loc))


(defn get-child-in
  "Like clojure.core/get-in, but for zipper structures."
  [loc key-path]
  (reduce get-child loc key-path))

(s/fdef loc-in
  :args (s/cat :root ::json/any
               :key-path ::key-path)
  :ret (s/nilable ::loc))

(defn loc-in
  "Convenience, like get-child-in, but it takes root and returns a loc or nil."
  [root key-path]
  (-> root json-zip (get-child-in key-path)))

(s/fdef stub-in
  :args (s/cat :loc ::loc
               :key-path ::json/key-path)
  :ret ::loc)

(defn stub-in
  "Given a loc an key path, stub out the path if it does not exist, returning
  a loc for the destination. If the loc does not exist, it will have the value
  ::stub. If incorrect keys are given for the data, will throw.
  If stub-in encounters an intermediate node of ::stub, it will replce it with
  the proper datastructure for the key path."
  [loc key-path]
  (let [node (z/node loc)]
    (if (map-entry? node)
      (recur (-> loc z/down z/right) key-path)
      (if-let [k (first key-path)]
        (if (or (coll? node) (= ::stub node))
          (do (assert (cond
                        (map? node) (string? k)
                        (coll? node) (number? k)
                        :else true) "Incorrect key type for node")
              (recur
               (if (= ::stub node)
                 (cond
                   (string? k)
                   (-> loc
                       (z/replace
                        (z/make-node loc
                                     {}
                                     [(clojure.lang.MapEntry. k
                                                              ::stub)]))
                       z/down)
                   (number? k)
                   (-> loc
                       (z/replace
                        (z/make-node loc
                                     []
                                     (repeat (inc k) ::stub)))
                       z/down
                       (->> (iterate z/right))
                       (nth k)))
                 (let [child-locs (take-while
                                   (complement nil?)
                                   (iterate z/right
                                            (z/down loc)))]
                   (if-let [[fk fv :as found] (find node k)]
                     (if (map? node)
                       (some
                        (fn [cl]
                          (when (= found (z/node cl))
                            cl))
                        child-locs)
                       (nth child-locs fk))
                     (if (map? node)
                       (-> loc
                           (z/append-child
                            (clojure.lang.MapEntry. k
                                                    ::stub))
                           z/down
                           z/rightmost)
                       (let [[lc rc] (split-at k child-locs)]
                         (-> loc
                             (z/replace
                              (z/make-node loc
                                           node
                                           (concat
                                            (map z/node lc)
                                            (repeat (- (inc k)
                                                       (count lc))
                                                      ::stub)
                                            (map z/node rc))))
                             z/down
                             (->> (iterate z/right))
                             (nth k)))))))
               (rest key-path)))
          (throw (ex-info "Can't path into a leaf node"
                          {:type ::cant-path-leaf-node
                           :loc loc
                           :key-path key-path})))
        loc))))

(s/def ::path-map
  (s/map-of
   ::key-path
   ::json/any))

(s/fdef json-locs
  :args (s/cat :json ::json/any)
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
  :args (s/cat :json ::json/any)
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
  :ret ::json/any)

(defn path-map->json
  [path-map]
  (get path-map []))
