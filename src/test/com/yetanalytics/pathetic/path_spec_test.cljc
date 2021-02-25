(ns com.yetanalytics.pathetic.path-spec-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.pathetic.json-path :as json-path]
            [com.yetanalytics.pathetic.path-spec :refer [spec-map
                                                         path->spec*
                                                         path->spec]])
  #?(:clj (:require [com.yetanalytics.pathetic-test-macros
                     :refer [read-long-statement]])
     :cljs (:require-macros [com.yetanalytics.pathetic-test-macros
                             :refer [read-long-statement]])))

(deftest spec-map-test
  (is (s/valid? :com.yetanalytics.pathetic.path-spec/spec-map spec-map)))

(def long-statement (read-long-statement))

(def path-value-map
  (reduce (fn [acc {jsn :json pth :path}] (assoc acc pth jsn))
          {}
          (json-path/path-seqs long-statement ['.. '*])))

(deftest path->spec*-test
  (testing "path->spec* works for lots of paths"
    ;; Use recursive descent to get all possible path-to-json pairs,
    ;; and use path->spec on each of those pairs.
    (is (every? (fn [[path v]]
                  (let [spec (path->spec* ::xs/statement path long-statement)]
                    (and spec (s/valid? spec v))))
                path-value-map)))
  (testing "path->spec works for arbitrary and relative paths"
    (is (= ::xs/language-map-text
           (path->spec* ::xs/activity ["definition" "name" "en-US"]))))
  (testing "path->spec can return functions for use as specs"
    (is (= string?
           (path->spec* ::xs/statement
                       ["object" "definition" "correctResponsesPattern" 0])))))

(deftest path->spec-test
  (testing "path->spec works like path->spec*"
    (is (= ::xs/language-map-text
           (path->spec ::xs/activity "$.definition.name.en-US")))
    (is (= string?
           (path->spec ::xs/statement
                       "$.object.definition.correctResponsesPattern[0]"))))
  (testing "Wildcards and unions get conformed"
    (is (= (path->spec ::xs/activity
                       "$.definition.name.en-US")
           (path->spec ::xs/activity
                       "$.definition.name.en-US|$.object.definition.correctResponsesPattern[0]")))
    (is (= (path->spec ::xs/statement
                       "$.object.definition.correctResponsesPattern[0]")
           (path->spec ::xs/statement
                       "$.object.definition.correctResponsesPattern[*]")))
    (is (= (path->spec ::xs/statement
                       "$.object.definition.correctResponsesPattern[0]")
           (path->spec ::xs/statement
                       "$.object.definition.correctResponsesPattern[0,1,2,3,4,5]")))))