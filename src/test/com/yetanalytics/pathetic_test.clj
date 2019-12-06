(ns com.yetanalytics.pathetic-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.pathetic :refer :all]
            [clojure.java.io :as io]
            [clojure.data.json :as json]))

(deftest satisfied-test
  (let [json-path [#{"foo"} #{"bar"} '* #{"quxx"} #{0 1}]
        key-path ["foo" "bar" "baz" "quxx" 0]]
    (testing "when json-path and key path match"
      (testing "returns the json path"
        (is (= json-path (satisfied json-path key-path)))))
    (testing "when json-path and key path match partially"
      (testing "returns the json path"
        (is (= (take 3 json-path) (take 3 (satisfied json-path key-path))))))
    (testing "when json-path and key path diverge"
      (testing "returns nil"
        (is (nil? (satisfied json-path (assoc key-path 3 "blork"))))))))

(def long-statement
  (with-open [r (io/reader (io/resource "pathetic/data/long.json"))]
    (json/read r)))

(deftest select-test
  (are [path selection]
      (= (select long-statement path)
         selection)
    "$.id" {"id" "6690e6c9-3ef0-4ed3-8b37-7f3964730bee"}
    "$.timestamp" {"timestamp" "2013-05-18T05:32:34.804Z"}

    "$.context.contextActivities.grouping[*]"
    {"context" {"contextActivities" {}}}

    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/sessionid']"
    {"context" {}}

    "$.result.score"
    {"result" {}}

    "$.result.success"
    {"result" {"success" true}}

    "$.result.completion"
    {"result" {"completion" true}}

    "$.context.contextActivities.category[*].id"
    {"context"
     {"contextActivities"
      {"category"
       [{"id" "http://www.example.com/meetings/categories/teammeeting"}]}}}

    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchmode']"
    {"context" {}}

    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchurl']"
    {"context" {}}

    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/moveon']"
    {"context" {}}

    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchparameters']"
    {"context" {}}

    "$.result.duration"
    {"result" {"duration" "PT1H0M0S"}}

    "$.result['https://w3id.org/xapi/cmi5/result/extensions/reason']"
    {"result" {}}

    "$.object.definition.type"
    {"object"
     {"definition" {"type" "http://adlnet.gov/expapi/activities/meeting"}}}))
