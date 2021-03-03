(ns com.yetanalytics.pathetic-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.test.check]
            [clojure.test.check.properties]
            [clojure.spec.test.alpha :as stest]
            [com.yetanalytics.pathetic :as p])
  #?(:clj (:require [com.yetanalytics.pathetic-test-macros
                     :refer [read-long-statement
                             parse-failed?
                             strict-parse-failed?]])
     :cljs (:require-macros [com.yetanalytics.pathetic-test-macros
                             :refer [read-long-statement
                                     parse-failed?
                                     strict-parse-failed?]])))

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

(def long-statement (read-long-statement))

;; Most parse testing is found in pathetic/json-path-test
(deftest parse-path-test
  (testing "Parsing JSONPath strings"
    (is (= [[["store"]]]
           (p/parse-path "$['store']")))
    (is (= [[["store"]] [["book"] [0 1]]]
           (p/parse-path "$['store']|$.book[0,1]")))
    (is (= [[["store"]]]
           (p/parse-path "$['store']|$.book[0,1]" {:first? true})))
    (is (parse-failed? (p/parse-path "$foobar")))
    (is (strict-parse-failed? (p/parse-path "$..*"
                                            {:strict? true})))
    (is (strict-parse-failed? (p/parse-path "$['store'][-1]"
                                            {:strict? true})))
    (is (strict-parse-failed? (p/parse-path "$['store'][0:5:1]"
                                            {:strict? true})))
    (is (strict-parse-failed? (p/parse-path "$['store'][0:5:1]"
                                            {:first? true :strict? true})))))

(deftest get-paths-test
  (testing "Enumerate deterministic JSONPaths"
    (is (= []
           (p/get-paths nil "$.foo")))
    (is (= [[]]
           (p/get-paths nil "$.foo" {:return-missing? true})))
    (is (= [["universe" 0 "foo" "bar"]]
           (p/get-paths {"universe" [{"foo" {"bar" 0}} {"baz" 1}]}
                        "$.universe.*.foo.bar")))
    (is (= [["universe" 1 "foo"] ;; Why is the order reversed?
            ["universe" 0 "foo" "bar"]]
           (p/get-paths {"universe" [{"foo" {"bar" 0}} {"baz" 1}]}
                        "$.universe.*.foo.bar"
                        {:return-missing? true})))
    (is (= [["store" "book" 0 "author"]
            ["store" "book" 1 "author"]
            ["store" "book" 2 "author"]
            ["store" "book" 3 "author"]]
           (p/get-paths goessner-ex "$.store.book[*].author")))
    (is (= [["store" "book" 0 "isbn"]
            ["store" "book" 1 "isbn"]
            ["store" "book" 2 "isbn"]
            ["store" "book" 3 "isbn"]]
           (p/get-paths goessner-ex "$.store.book[*].isbn" {:return-missing? true})))
    (is (= [["store" "book" 2 "isbn"]
            ["store" "book" 3 "isbn"]]
           (p/get-paths goessner-ex "$.store.book[*].isbn" {:return-missing? false})))
    (is (= [["store" "bicycle" "color"]
            ["store" "bicycle" "price"]]
           (p/get-paths goessner-ex "$.store.bicycle..*")))
    (is (= []
           (p/get-paths long-statement
                        "$.context.contextActivities.grouping[*]")))
    (is (= [["context" "contextActivities" "grouping"]]
           (p/get-paths long-statement
                        "$.context.contextActivities.grouping[*]"
                        {:return-missing? true})))
    (is (= [["context" "contextActivities" "grouping"]]
           (p/get-paths long-statement
                        "$.context.contextActivities.grouping[0]"
                        {:return-missing? true})))
    (is (= [["context" "contextActivities" "category" 0 "id"]]
           (p/get-paths long-statement
                        "$.context.contextActivities.category[*].id")))))

(deftest get-values-test-1
  (testing "Testing JSONPath on example provided by Goessner"
    (are [expected path]
         (= expected (p/get-values goessner-ex path {:return-missing? true}))
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

(deftest get-values-test-2
  (testing "Testing JSONPath on example Statement"
    ;; Hits
    (are [expected path]
         (= expected (p/get-values long-statement path {:return-missing? true}))
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
      [true true]
      "$.result['success', 'completion']"
      ["http://www.example.com/meetings/categories/teammeeting"]
      "$.context.contextActivities.category[*].id")
    ;; Misses
    (are [path]
         (= [nil] (p/get-values long-statement path {:return-missing? true}))
      "$.context.contextActivities.grouping[*]"
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/sessionid']"
      "$.result.score"
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchmode']"
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchurl']"
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/moveon']"
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchparameters']"
      "$.result['https://w3id.org/xapi/cmi5/result/extensions/reason']")))

(deftest get-path-value-map-test
  (testing "Testing getting JSONPath-to-JSON maps"
    (is (= {}
           (p/get-path-value-map nil "$.foo")))
    (is (= {[] nil}
           (p/get-path-value-map nil "$.foo" {:return-missing? true})))
    (is (= {["foo"] nil}
           (p/get-path-value-map {"foo" nil} "$.foo")))
    (is (= {["store" "book" 0 "author"] "Nigel Rees"}
           (p/get-path-value-map goessner-ex "$.store.book[0].author")))
    (is (= {["store" "book" 0 "author"] "Nigel Rees"
            ["store" "book" 2 "author"] "Herman Melville"}
           (p/get-path-value-map goessner-ex "$.store.book[0,2].author")))
    (is (= {["store" "book" 0 "author"] "Nigel Rees"
            ["store" "book" 2 "author"] "Herman Melville"}
           (p/get-path-value-map goessner-ex "  $.store.book[0].author
                                            | $.store.book[2].author")))
    (is (= {["store" "book" 4] nil}
           (p/get-path-value-map goessner-ex
                                 "$.store.book[4].author"
                                 {:return-missing? true})))
    (is (= {["context" "contextActivities" "category" 0 "id"]
            "http://www.example.com/meetings/categories/teammeeting"}
           (p/get-path-value-map long-statement
                                 "$.context.contextActivities.category[*].id")))))

(deftest select-keys-at-test
  (testing "Selecting keys with a JSONPath."
    (is (= {nil {}} (p/select-keys-at nil "$.foo" {:first? true})))
    (is (= {nil {}} (p/select-keys-at {} "$.foo" {:first? true})))
    (are [expected path]
         (= expected (p/select-keys-at goessner-ex path))
      [{"store" {"book" [{"author" "Nigel Rees"}
                         {"author" "Evelyn Waugh"}
                         {"author" "Herman Melville"}
                         {"author" "J.R.R. Tolkien"}]}}
       {"store" {"book" [{}
                         {}
                         {"isbn" "0-553-21311-3"}
                         {"isbn" "0-395-19395-8"}]}}]
      "$.store.book[*].author|$.store.book.*.isbn"
      ;; Each entry in the recursive descent result vector is assoc'd into the
      ;; same map
      [goessner-ex]
      "$..*")
    (are [expected path]
         (= expected (p/select-keys-at long-statement path {:first? true}))
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

(deftest excise-test
  (testing "Excising values using JSONPath"
    (is (= nil
           (p/excise nil "$.foo")))
    (is (= [:b :c]
           (p/excise [:a :b :c] "$[0]")))
    (is (= [:a :b :c]
           (p/excise [:a :b :c] "$[3]")))
    (is (= {"universe" [{} {}]}
           (p/excise {"universe" [{"foo" :a} {"foo" :b}]}
                     "$.universe.*.foo")))
    (is (= {}
           (p/excise {"universe" [{"foo" :a} {"foo" :b}]}
                     "$.universe.*.foo"
                     {:prune-empty? true})))
    (is (= {"universe" [{"bar" :b}]}
           (p/excise {"universe" [{"foo" :a} {"bar" :b}]}
                     "$.universe.*.foo"
                     {:prune-empty? true})))
    (is (= {"universe" [{"foo" {}} {}]}
           (p/excise {"universe" [{"foo" {"bar" :a}} {"bar" :b}]}
                     "$.universe..bar")))
    (is (= {}
           (p/excise {"universe" [{"foo" {"bar" :a}} {"bar" :b}]}
                     "$.universe..bar"
                     {:prune-empty? true})))
    (is (= {"universe" [{"foo" {}} {"baz" :c}]}
           (p/excise {"universe" [{"foo" {"bar" :a}} {"bar" :b "baz" :c}]}
                     "$.universe..bar")))
    (is (= {"universe" [{"baz" :c}]}
           (p/excise {"universe" [{"foo" {"bar" :a}} {"bar" :b "baz" :c}]}
                     "$.universe..bar"
                     {:prune-empty? true})))
    (is (= {"store" {"book"    [{"author" "Nigel Rees"}
                                {"author" "Evelyn Waugh"}
                                {"author" "Herman Melville"}
                                {"author" "J.R.R. Tolkien"}]
                     "bicycle" {"color" "red"
                                "price" 20}}}
           (p/excise goessner-ex "  $['store']['book'][*]['category']
                              | $['store']['book'][*]['title']
                              | $['store']['book'][*]['price']
                              | $['store']['book'][*]['isbn']")))
    (is (= (dissoc long-statement "id")
           (p/excise long-statement "$.id")))
    (is (= (update-in long-statement
                      ["context" "contextActivities" "category" 0]
                      dissoc
                      "id")
           (p/excise long-statement
                     "$.context.contextActivities.category[*].id")))
    (is (= long-statement ;; Don't excise missing elements
           (p/excise long-statement
                     "$.context.contextActivities.grouping[*]")))))

(deftest apply-value-test
  (testing "Applying and updating values using JSONPath"
    (is (= {"foo" :a}
           (p/apply-value nil "$.foo" :a)))
    (are [expected path]
         (= expected
            (p/apply-value {"universe" [{"foo" :a} {"bar" :b}]} path :c))
      {"universe" [{"foo" :a} {"bar" :b "baz" :c}]}       "$.universe[1].baz"
      {"universe" [{"foo" :a} {"bar" :c}]}                "$.universe[1].bar"
      {"universe" [{"foo" :a} {"bar" [:c]}]}              "$.universe[1].bar.*"
      {"universe" [{"foo" :a} {"bar" [:c]}]}              "$.universe[1].bar[0]"
      {"universe" [{"foo" :a} {"bar" [nil :c]}]}          "$.universe[1].bar[1]"
      {"universe" [{"foo" :a} {"bar" [:c nil :c]}]}       "$.universe[1].bar[0,2]"
      {"universe" [{"foo" :a} {"bar" :b "1" :c}]}         "$.universe[1].*"
      {"universe" [{"foo" :a} {"bar" :b} {"baz" :c}]}     "$.universe[*].baz"
      {"universe" [{"foo" :a "b" :c} {"bar" :b "b" :c}]}  "$.universe[0,1].b"
      {"universe" [:c :c]}                                "$.universe[0,1]"
      {"universe" :c "baz" :c}                            "$['universe','baz']"
      {"universe" :c "baz" :c}                            "$.universe|$.baz"
      {"universe" [{"foo" :a} {"bar" :b}] "baz" :c}       "$.baz"
      {"universe" [{"foo" :a} {"bar" :b}] "1" {"baz" :c}} "$.*.baz"
      {"universe" [{"foo" :a} {"bar" :b}] "1" :c}         "$.*"
      [:c]                                                "$[0]"
      nil                                                 "$")
    (is (strict-parse-failed? (p/apply-value {"universe" :a} "$..*" :c)))
    ;; Tests on Statement
    (is (= (assoc long-statement "foo" "bar")
           (p/apply-value long-statement "$.foo" "bar")))
    (is (= (update-in long-statement
                      ["context" "contextActivities" "category"]
                      (fn [old]
                        (conj old
                              {"id" "http://www.example.com/meetings/categories/brainstorm_sesh"}
                              {"id" "http://www.example.com/meetings/categories/whiteboard_sesh"})))
           (-> long-statement
               (p/apply-value "$.context.contextActivities.category[*].id"
                              "http://www.example.com/meetings/categories/brainstorm_sesh")
               (p/apply-value "$.context.contextActivities.category[*].id"
                              "http://www.example.com/meetings/categories/whiteboard_sesh"))))))

(deftest gen-tests
  (testing "Generative tests for pathetic"
    (let [results
          (stest/check `[#_p/get-paths* ;; Bad!
                         #_p/get-values* ;; Good!
                         #_p/get-path-value-map*
                         #_p/select-keys-at* ;; Bad!
                         #_p/excise* ;; Exception!
                         p/apply-value* ;; Infinite loop!
                         ]
                       {:clojure.spec.test.check/opts
                        {:num-tests #?(:clj 100 :cljs 50)
                         :seed (rand-int 2000000000)}})
          {:keys [total check-passed]}
          (stest/summarize-results results)]
      (is (= total check-passed)))))

(comment
  (p/get-paths* {} [[[-1]]] {:return-missing? true})
  (p/get-values* {} [[[-1]]] {:return-missing? true})

  (p/select-keys-at* {} [[[""]]]))
