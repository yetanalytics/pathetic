(ns com.yetanalytics.pathetic
  (:require [com.yetanalytics.pathetic.json-path :as json-path]
            [com.yetanalytics.pathetic.zip :as zip]
            [clojure.zip :as z]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.pathetic.json :as json]))

(s/fdef satisfied
  :args (s/cat :json-path ::json-path/json-path
               :key-path ::zip/key-path)
  :ret (s/nilable ::json-path/json-path))

(defn satisfied
  "Given a json path and a key path, return nil if they diverge, or if they partially
  match return a seq of the covered pattern"
  [path key-path]
  (when-let [partial-sat
             (map first
                  (take-while
                   (fn [[p pk]]
                     (cond
                       (= p '*) true
                       (set? p) (contains? p pk)
                       :else (let [{:keys [start
                                           end
                                           step]} p]
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

(s/fdef select
  :args (s/cat :path string?
               :json ::json/any)
  :ret ::json/any)

(defn select
  "Given json data and a path, return the selection. Note that this does not
  return the json-path selection, just the pruned datastructure as with
  clojure.core/select-keys"
  [json path]
  (let [path (json-path/parse path)]
    (loop [loc (zip/json-zip json)]
      (if (z/end? loc)
        (z/root loc)
        (if (zip/internal? loc)
          (recur (z/next loc))
          (let [key-path (zip/k-path loc)]
            (if-let [sat (satisfied path key-path)]
              ;; if we have satisfied at least some of the spec, we
              ;; want to keep going
              (recur (z/next loc))
              ;; if it doesn't match, kill this or any internal nodes
              ;; leading to it.
              (let [ploc (z/up loc)
                    pnode (z/node ploc)]
                (recur
                 (z/remove
                  (if (map-entry? pnode)
                    ploc
                    loc)))))))))))
