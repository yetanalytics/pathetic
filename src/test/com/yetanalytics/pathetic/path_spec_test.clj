(ns com.yetanalytics.pathetic.path-spec-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.java.io    :as io]
            [clojure.data.json  :as json]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.pathetic.json-path :as json-path]
            [com.yetanalytics.pathetic.path-spec
             :refer [spec-map path->spec]]))

(deftest spec-map-test
  (is (s/valid? :com.yetanalytics.pathetic.path-spec/spec-map spec-map)))

(def long-statement
  (with-open
   [r (io/reader (io/resource "pathetic/data/long.json"))]
    (json/read r)))

(def path-value-map
  (reduce (fn [acc {jsn :json pth :path}] (assoc acc pth jsn))
          {}
          (json-path/path-seqs long-statement ['.. '*])))

(deftest path->spec-test
  (testing "path->spec works for lots of paths"
    ;; Use recursive descent to get all possible path-to-json pairs,
    ;; and use path->spec on each of those pairs.
    (is (every? (fn [[path v]]
                  (let [spec (path->spec ::xs/statement path long-statement)]
                    (and spec (s/valid? spec v))))
                path-value-map)))
  (testing "path->spec works for arbitrary and relative paths"
    (is (= ::xs/language-map-text
           (path->spec ::xs/activity ["definition" "name" "en-US"]))))
  (testing "path->spec can return functions for use as specs"
    (is (= string?
           (path->spec ::xs/statement
                       ["object" "definition" "correctResponsesPattern" 0])))))
