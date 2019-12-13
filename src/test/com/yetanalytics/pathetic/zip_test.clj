(ns com.yetanalytics.pathetic.zip-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.pathetic.zip :refer :all]
            [clojure.zip :as z]
            [clojure.test.check :as tc]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer :all]))

(deftest zip-functions-test
  (let [results (stest/check
                 `#{json-zip
                    internal?
                    el-key
                    k-path
                    get-child
                    get-child-in
                    loc-in
                    json-locs
                    json->path-map
                    path-map->json
                    })
        {:keys [total
                check-passed]} (stest/summarize-results results)]
    (is (= total check-passed))))

(defspec k-path-is-valid-for-get-in
  (prop/for-all
   [json (s/gen :com.yetanalytics.pathetic.zip/any-json)]
   (let [locs (->> json
                   json-zip
                   (iterate z/next)
                   (take-while (complement z/end?))
                   ;; don't look at map entries
                   (remove internal?))]
     (every?
      (fn [loc]
        (let [key-path (k-path loc)]
          (= (z/node loc)
             (get-in json key-path)
             (z/node (loc-in json key-path)))))
      locs))))

(comment
  (keys (first (stest/check (stest/checkable-syms 'com.yetanalytics.pathetic.zip))))
  (stest/instrumentable-syms `com.yetanalytics.pathetic.zip)
  (s/exercise-fn `com.yetanalytics.pathetic.zip/json->path-map)
  #{com.yetanalytics.pathetic.zip/json->path-map com.yetanalytics.pathetic.zip/path-map->json com.yetanalytics.pathetic.zip/k-path com.yetanalytics.pathetic.zip/el-key com.yetanalytics.pathetic.zip/internal? com.yetanalytics.pathetic.zip/path-map com.yetanalytics.pathetic.zip/json-zip}


  )
