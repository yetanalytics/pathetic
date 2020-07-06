(ns com.yetanalytics.pathetic-test
  (:require [clojure.test                        :refer :all]
            [com.yetanalytics.pathetic           :refer :all]
            [clojure.java.io                     :as io]
            [clojure.data.json                   :as json]
            [com.yetanalytics.pathetic.json-path :as json-path]))

(def long-statement
  (with-open [r (io/reader (io/resource "pathetic/data/long.json"))]
    (json/read r)))

(deftest select-keys-at-test
  (are [path selection]
      (= (select-keys-at long-statement path)
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

(deftest get-at-test
  (are [path selection]
      (= (get-at long-statement (json-path/parse path))
         selection)
    ;; hits
    "$.id"                                       ["6690e6c9-3ef0-4ed3-8b37-7f3964730bee"]
    "$.timestamp"                                ["2013-05-18T05:32:34.804Z"]
    "$.object.definition.type"                   ["http://adlnet.gov/expapi/activities/meeting"]
    "$.result.duration"                          ["PT1H0M0S"]
    "$.result.success"                           [true]
    "$.result.completion"                        [true]
    "$.context.contextActivities.category[*].id" ["http://www.example.com/meetings/categories/teammeeting"]
    ;; misses
    "$.context.contextActivities.grouping[*]"                                                []
    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/sessionid']"        []
    "$.result.score"                                                                         []
    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchmode']"       []
    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchurl']"        []
    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/moveon']"           []
    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchparameters']" []
    "$.result['https://w3id.org/xapi/cmi5/result/extensions/reason']"                        []))

(deftest excise-test
  (is (= (dissoc long-statement "id")
         (excise long-statement (json-path/parse "$.id")))))

(deftest apply-values-test
  (is (= (assoc long-statement "foo" "bar")
         (apply-values long-statement
                       (json-path/parse "$.foo")
                       "bar")))
  (is (= (update-in long-statement ["context" "contextActivities" "category"]
                    (fn [old] (conj old
                                    {"id" "http://www.example.com/meetings/categories/brainstorm_sesh"}
                                    {"id" "http://www.example.com/meetings/categories/whiteboard_sesh"})))
         (apply-values long-statement
                       (json-path/parse "$.context.contextActivities.category[*].id")
                       ["http://www.example.com/meetings/categories/brainstorm_sesh"
                        "http://www.example.com/meetings/categories/whiteboard_sesh"]))))
