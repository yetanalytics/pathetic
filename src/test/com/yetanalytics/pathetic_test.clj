(ns com.yetanalytics.pathetic-test
  (:require [clojure.test                        :refer :all]
            [com.yetanalytics.pathetic           :refer :all]
            [clojure.java.io                     :as io]
            [clojure.data.json                   :as json]
            [com.yetanalytics.pathetic.json-path :as json-path]))

(def example-1 {"store"
                {"book"
                 [{"category" "reference"
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
                 "bicycle"
                 {"color" "red"
                  "price" 20}}})

(deftest read-json-test-1
  (testing "Testing JSONPath on example provided by Goessner"
    (are [expected path]
         (= expected (get-at example-1 path :return-missing? true))
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

(def long-statement
  (with-open [r (io/reader (io/resource "pathetic/data/long.json"))]
    (json/read r)))

(select-keys-at-2 long-statement
                  [#{"result"} #{"score"}])

(select-keys-at-2 long-statement
                  [#{"object"}
                   #{"definition"}
                   #{"type"}])

(select-keys-at-2 long-statement
                  [#{"context"}
                   #{"contextActivities"}
                   #{"grouping"}
                   '*])

(json-path/path-seqs long-statement
                     [#{"context"} #{"contextActivities"} #{"grouping"} #{"foobar"}])


(deftest select-keys-at-test
  (are [path selection]
       (= (select-keys-at-2 long-statement (json-path/parse-first path))
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
       (= (get-at-2 long-statement (json-path/parse-first path))
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
