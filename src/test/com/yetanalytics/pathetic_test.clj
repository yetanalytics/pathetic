(ns com.yetanalytics.pathetic-test
  (:require [clojure.test                        :refer :all]
            [com.yetanalytics.pathetic           :refer :all]
            [clojure.java.io                     :as io]
            [clojure.data.json                   :as json]
            [com.yetanalytics.pathetic.json-path :as json-path]))

(def goessner-ex {"store" {"book"    [{"category" "reference"
                                       "author"   "Nigel Rees"
                                       "title"    "Sayings of the Century"
                                       "price"    8.95}
                                      {"category" "fiction"
                                       "author"   "Evelyn Waugh"
                                       "title"    "Sword of Honour"
                                       "price"    12.99}
                                      {"category" "fiction"
                                       "author"   "Herman Melville"
                                       "title"    "Moby Dick"
                                       "isbn"     "0-553-21311-3"
                                       "price"    8.99}
                                      {"category" "fiction"
                                       "author"   "J.R.R. Tolkien"
                                       "title"    "The Lord of the Rings"
                                       "isbn"     "0-395-19395-8"
                                       "price"    22.99}]
                           "bicycle" {"color" "red"
                                      "price" 20}}})

(def long-statement
  (with-open [r (io/reader (io/resource "pathetic/data/long.json"))]
    (json/read r)))

;; Most parse testing is found in pathetic/json-path-test
(deftest parse-test
  (testing "Parsing JSONPath strings"
    (is (= [[["store"]]]
           (parse "$['store']")))
    (is (= [[["store"]] [["book"] [0 1]]]
           (parse "$['store']|$.book[0,1]")))
    (is (= [["store"]]
           (parse "$['store']|$.book[0,1]" :first? true)))
    (is (thrown-with-msg? Exception
                          #"Cannot parse JSONPath string"
                          (parse "$foobar")))))

(deftest enumerate-test
  (testing "Enumerate deterministic JSONPaths"
    (is (= [["universe" 0 "foo" "bar"]]
           (enumerate {"universe" [{"foo" {"bar" 0}} {"baz" 1}]}
                      "$.universe.*.foo.bar")))
    (is (= [["universe" 1 "foo"] ;; Why is the order reversed?
            ["universe" 0 "foo" "bar"]]
           (enumerate {"universe" [{"foo" {"bar" 0}} {"baz" 1}]}
                      "$.universe.*.foo.bar"
                      :return-missing? true)))
    (is (= [["store" "book" 0 "author"]
            ["store" "book" 1 "author"]
            ["store" "book" 2 "author"]
            ["store" "book" 3 "author"]]
           (enumerate goessner-ex "$.store.book[*].author")))
    (is (= [["store" "book" 0 "isbn"]
            ["store" "book" 1 "isbn"]
            ["store" "book" 2 "isbn"]
            ["store" "book" 3 "isbn"]]
           (enumerate goessner-ex "$.store.book[*].isbn" :return-missing? true)))
    (is (= [["store" "book" 2 "isbn"]
            ["store" "book" 3 "isbn"]]
           (enumerate goessner-ex "$.store.book[*].isbn" :return-missing? false)))
    (is (= [["store" "bicycle" "color"]
            ["store" "bicycle" "price"]]
           (enumerate goessner-ex "$.store.bicycle..*")))
    (is (= []
           (enumerate long-statement
                      "$.context.contextActivities.grouping[*]")))
    (is (= [["context" "contextActivities" "grouping"]]
           (enumerate long-statement
                      "$.context.contextActivities.grouping[*]"
                      :return-missing? true)))
    (is (= [["context" "contextActivities" "grouping"]]
           (enumerate long-statement
                      "$.context.contextActivities.grouping[0]"
                      :return-missing? true)))
    (is (= [["context" "contextActivities" "category" 0 "id"]]
           (enumerate long-statement
                      "$.context.contextActivities.category[*].id")))))

(deftest get-at-test-1
  (testing "Testing JSONPath on example provided by Goessner"
    (are [expected path]
         (= expected (get-at goessner-ex path :return-missing? true))
      ; The authors of all books in the store
      ["Nigel Rees" "Evelyn Waugh" "Herman Melville" "J.R.R. Tolkien"]
      "$.store.book[*].author"
      ["Nigel Rees" "Evelyn Waugh" "Herman Melville" "J.R.R. Tolkien"]
      "$..author"
      ["Nigel Rees" "Evelyn Waugh" "Herman Melville" "J.R.R. Tolkien"]
      "$.store.book.*.author"
      ["Nigel Rees" "Evelyn Waugh" "Herman Melville" "J.R.R. Tolkien"]
      "$['store']['book'][*]['author']"
      ; The first author in the store
      ["Nigel Rees"]
      "$.store.book[0].author"
      ; All things in the store
      [[{"category" "reference"
         "author"   "Nigel Rees"
         "title"    "Sayings of the Century"
         "price"    8.95}
        {"category" "fiction"
         "author"   "Evelyn Waugh"
         "title"    "Sword of Honour"
         "price"    12.99}
        {"category" "fiction"
         "author"   "Herman Melville"
         "title"    "Moby Dick"
         "isbn"     "0-553-21311-3"
         "price"    8.99}
        {"category" "fiction"
         "author"   "J.R.R. Tolkien"
         "title"    "The Lord of the Rings"
         "isbn"     "0-395-19395-8"
         "price"    22.99}]
       {"color" "red"
        "price" 20}]
      "$.store.*"
      ; The price of everything in the store
      [8.95 12.99 8.99 22.99 20]
      "$.store..price"
      [8.95 12.99 8.99 22.99 20]
      "$..price"
      ; The price of only books in the store
      [8.95 12.99 8.99 22.99]
      "$.store.book[*].price"
      ; Book ISBNs
      [nil nil "0-553-21311-3" "0-395-19395-8"]
      "$.store.book.*.isbn"
      ; The third book
      [{"category" "fiction"
        "author"   "Herman Melville"
        "title"    "Moby Dick"
        "isbn"     "0-553-21311-3"
        "price"    8.99}]
      "$..book[2]"
      [{"category" "fiction"
        "author"   "Herman Melville"
        "title"    "Moby Dick"
        "isbn"     "0-553-21311-3"
        "price"    8.99}]
      "$..book[-2]"
      ; The last book via subscript array slice
      [{"category" "fiction"
        "author"   "J.R.R. Tolkien"
        "title"    "The Lord of the Rings"
        "isbn"     "0-395-19395-8"
        "price"    22.99}]
      "$..book[-1:]"
      ; Last two books
      [{"category" "fiction"
        "author"   "Herman Melville"
        "title"    "Moby Dick"
        "isbn"     "0-553-21311-3"
        "price"    8.99}
       {"category" "fiction"
        "author"   "J.R.R. Tolkien"
        "title"    "The Lord of the Rings"
        "isbn"     "0-395-19395-8"
        "price"    22.99}]
      "$..book[-2:]"
      ; The first and third books via subscript union
      [{"category" "reference"
        "author"   "Nigel Rees"
        "title"    "Sayings of the Century"
        "price"    8.95}
       {"category" "fiction"
        "author"   "Herman Melville"
        "title"    "Moby Dick"
        "isbn"     "0-553-21311-3"
        "price"    8.99}]
      "$..book[0,2]"
      ; The first two books via slice
      [{"category" "reference"
        "author"   "Nigel Rees"
        "title"    "Sayings of the Century"
        "price"    8.95}
       {"category" "fiction"
        "author"   "Evelyn Waugh"
        "title"    "Sword of Honour"
        "price"    12.99}]
      "$..book[:2]"
      [{"category" "reference"
        "author"   "Nigel Rees"
        "title"    "Sayings of the Century"
        "price"    8.95}
       {"category" "fiction"
        "author"   "Evelyn Waugh"
        "title"    "Sword of Honour"
        "price"    12.99}]
      "$..book[0:2]"
      ; Unmatchable values
      [nil]
      "$.non-existent"
      [nil]
      "$.stire.book.*.author"
      [nil]
      "$.store.book[4]"
      [nil]
      "$.store.book[-5]"
      [nil nil nil nil]
      "$.store.book[*].blah")))

(deftest get-at-test-2
  (testing "Testing JSONPath on example Statement"
    ;; Hits
    (are [expected path]
         (= expected (get-at long-statement path :return-missing? true))
      ["6690e6c9-3ef0-4ed3-8b37-7f3964730bee"]
      "$.id"
      ["2013-05-18T05:32:34.804Z"]
      "$.timestamp"
      ["http://adlnet.gov/expapi/activities/meeting"]
      "$.object.definition.type"
      ["PT1H0M0S"]
      "$.result.duration"
      [true]
      "$.result.success"
      [true]
      "$.result.completion"
      ["http://www.example.com/meetings/categories/teammeeting"]
      "$.context.contextActivities.category[*].id")
    ;; Misses
    (are [path]
         (= [nil] (get-at long-statement path :return-missing? true))
      "$.context.contextActivities.grouping[*]"
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/sessionid']"
      "$.result.score"
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchmode']"
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchurl']"
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/moveon']"
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchparameters']"
      "$.result['https://w3id.org/xapi/cmi5/result/extensions/reason']")))

(deftest select-keys-at-test
  (testing "Selecting keys with a JSONPath."
    (are [expected path]
         (= expected (select-keys-at goessner-ex path))
      [{"store" {"book" [{"author" "Nigel Rees"}
                         {"author" "Evelyn Waugh"}
                         {"author" "Herman Melville"}
                         {"author" "J.R.R. Tolkien"}]}}
       {"store" {"book" [{}
                         {}
                         {"isbn" "0-553-21311-3"}
                         {"isbn" "0-395-19395-8"}]}}]
      "$.store.book[*].author|$.store.book.*.isbn")
    (are [expected path]
         (= expected (select-keys-at long-statement path :first? true))
      {"id" "6690e6c9-3ef0-4ed3-8b37-7f3964730bee"}
      "$.id"
      {"timestamp" "2013-05-18T05:32:34.804Z"}
      "$.timestamp"
      {"context" {"contextActivities" {}}}
      "$.context.contextActivities.grouping[*]"
      {"context" {}}
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/sessionid']"
      {"result" {}}
      "$.result.score"
      {"result" {"success" true}}
      "$.result.success"
      {"result" {"completion" true}}
      "$.result.completion"
      {"context"
       {"contextActivities"
        {"category"
         [{"id" "http://www.example.com/meetings/categories/teammeeting"}]}}}
      "$.context.contextActivities.category[*].id"
      {"context" {}}
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchmode']"
      {"context" {}}
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchurl']"
      {"context" {}}
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/moveon']"
      {"context" {}}
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchparameters']"
      {"result" {"duration" "PT1H0M0S"}}
      "$.result.duration"
      {"result" {}}
      "$.result['https://w3id.org/xapi/cmi5/result/extensions/reason']"
      {"object"
       {"definition" {"type" "http://adlnet.gov/expapi/activities/meeting"}}}
      "$.object.definition.type")))

;; (deftest excise-test
;;   (is (= (dissoc long-statement "id")
;;          (excise long-statement (json-path/parse "$.id")))))

;; (deftest apply-values-test
;;   (is (= (assoc long-statement "foo" "bar")
;;          (apply-values long-statement
;;                        (json-path/parse "$.foo")
;;                        "bar")))
;;   (is (= (update-in long-statement ["context" "contextActivities" "category"]
;;                     (fn [old] (conj old
;;                                     {"id" "http://www.example.com/meetings/categories/brainstorm_sesh"}
;;                                     {"id" "http://www.example.com/meetings/categories/whiteboard_sesh"})))
;;          (apply-values long-statement
;;                        (json-path/parse "$.context.contextActivities.category[*].id")
;;                        ["http://www.example.com/meetings/categories/brainstorm_sesh"
;;                         "http://www.example.com/meetings/categories/whiteboard_sesh"]))))
