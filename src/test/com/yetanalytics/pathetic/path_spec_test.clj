(ns com.yetanalytics.pathetic.path-spec-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.pathetic.path-spec :refer :all]
            [xapi-schema.spec :as xs]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.pathetic.zip :as pzip]
            [clojure.java.io :as io]
            [clojure.data.json :as json]))

(deftest spec-map-test
  (is (s/valid? :com.yetanalytics.pathetic.path-spec/spec-map spec-map)))

(def long-statement
  (with-open
    [r (io/reader (io/resource "pathetic/data/long.json"))]
    (json/read r)))

(deftest path->spec-test
  (testing "works for lots of paths"
    ;; Explode a statement using a helper from our zipperoo to get a bunch of
    ;; paths and leaf values
    (is (every?
         (fn [[path v]]
           (let [spec (path->spec ::xs/statement path long-statement)]
             (and spec
                  (s/valid? spec v))))
         (pzip/json->path-map long-statement))))
  (testing "works for arbitrary and relative paths"
    (is (= ::xs/language-map-text
           (path->spec ::xs/activity ["definition" "name" "en-US"]))))
  (testing "can return functions for use as specs"
    (is (= string?
           (path->spec ::xs/statement
                       ["object" "definition" "correctResponsesPattern" 0])))))
