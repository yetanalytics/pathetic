(ns com.yetanalytics.pathetic-standards-test
  (:require [clojure.test :refer [deftest testing is]]
            [com.yetanalytics.pathetic :refer [get-values]])
  #?(:clj (:require [com.yetanalytics.pathetic-test-macros
                     :refer [parse-failed?]])
     :cljs (:require-macros [com.yetanalytics.pathetic-test-macros
                             :refer [parse-failed?]])))

;; This test suite is designed to compare Pathetic against the JSONPath
;; examples given by Christoph Burgmer's test suite at:
;; https://cburgmer.github.io/json-path-comparison/
;; 
;; Unless otherwise specified, Pathetic attempts to follow the consensus given
;; by different JSONPath implementations. If not consensus exists, then
;; Pathetic will default to throwing a parse failure, if that is the reason
;; for lack of consensus.
;; 
;; Note: We do not test script/filter expressions as we do not support them.

(def store-example {"store" {"book"    [{"category" "reference"
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
                                        "price" 19.95}}})

(deftest get-at-test-dot
  (testing "get-at function on dot notation"
    (is (= ["value"]
           (get-values {"key" "value"} "$.key")))
    (is (= []
           (get-values [0 1] "$.key")))
    (is (= [nil]
           (get-values [0 1] "$.key" {:return-missing? true})))
    (is (= [["first" "second"]]
           (get-values {"key" ["first" "second"]} "$.key")))
    (is (= []
           (get-values [{"id" 2}] "$.key")))
    (is (= [nil]
           (get-values [{"id" 2}] "$.key" {:return-missing? true})))
    (is (= [{}]
           (get-values {"key" {}} "$.key")))
    (is (= [nil]
           (get-values {"key" nil} "$.key")))
    (is (= []
           (get-values {"key" "value"} "$.missing")))
    (is (= [nil]
           (get-values {"key" "value"} "$.missing" {:return-missing? true})))
    (is (= [42 200 500] ;; Original order: [200 42 500]
           (get-values {"k"   [{"key" "some value"}, {"key" 42}]
                        "kk"  [[{"key" 100} {"key" 200} {"key" 300}]
                               [{"key" 400} {"key" 500} {"key" 600}]]
                        "key" [0 1]}
                       "$..[1].key")))
    (is (= [1 1]
           (get-values [{"a" 1} {"a" 1}] "$[*].a")))
    (is (= [1]
           (get-values [{"a" 1} {"a" 1}] "$[*].a" {:return-duplicates? false})))
    (is (= [1]
           (get-values [{"a" 1}] "$[*].a")))
    (is (= [1]
           (get-values [{"a" 1} {"b" 1}] "$[*].a")))
    (is (= [nil 1] ;; Supposed to be [1 nil], but reversed due to using BFS instead of DFS
           (get-values [{"a" 1} {"b" 1}] "$[*].a" {:return-missing? true})))
    ;; Original order: ["russian dolls" "something" "top" "value" {"key" "russian dolls"}]
    (is (= ["top" "value" "something" {"key" "russian dolls"} "russian dolls"]
           (get-values {"object" {"key"   "value"
                                  "array" [{"key" "something"}
                                           {"key" {"key" "russian dolls"}}]}
                        "key"    "top"}
                       "$..key")))
    ;; Original order: [12.99 19.95 22.99 8.95 8.99]
    (is (= [8.95 12.99 8.99 22.99 19.95]
           (get-values store-example "$.store..price")))
    (is (= [8.95 12.99 8.99 22.99 19.95] ;; Recursive descent doesn't include missing leaves
           (get-values store-example "$.store..price" {:return-missing? true})))
    (is (= ["ey" "see"]
           (get-values [{"key" "ey"} {"key" "bee"} {"key" "see"}] "$[0,2].key")))
    (is (= ["value"]
           (get-values {"key-dash" "value"} "$.key-dash")))
    (is (parse-failed?
         (get-values {"key" "value" "\"key\"" 42} "$.\"key\"")))
    (is (parse-failed?
         (get-values {"key" 42 "" 9001 "''" "nice"} "$.")))
    ;; Keyword keys
    (is (= ["value"] (get-values {"in" "value"} "$.in")))
    (is (= ["value"] (get-values {"length" "value"} "$.length")))
    (is (= [] (get-values [4 5 6] "$.length")))
    (is (= ["value"] (get-values {"nil" "value"} "$.nil")))
    (is (= ["value"] (get-values {"true" "value"} "$.true")))
    (is (parse-failed?
         (get-values {"$" "value"} "$.$")))
    ;; Consensus for non-ASCII keys is ["value"], but we instead go with the JS
    ;; jsonpath lib and throw a parse error
    (is (parse-failed?
         (get-values {"上下" "value"} "$.上下")))
    (is (parse-failed?
         (get-values {"上下" "value"} "$.\u4E0A\u4E0B")))
    (is (= [] ;; No consensus, we treat "2" as an identifier
           (get-values ["first" "second" "third" "fourth" "fifth"] "$.2")))
    (is (= ["second"]
           (get-values {"a" "first" "2" "second" "c" "third"} "$.2")))
    (is (= [] ;; No consensus
           (get-values ["first" "second" "third" "fourth" "fifth"] "$.-1")))
    (is (parse-failed? ;; No consensus, JS result
         (get-values {"key" "value"  "'key'" 42} "$.'key'")))
    (is (parse-failed? ;; No consensus, JS result
         (get-values {"object" {"key"   "value"
                                "'key'" 100
                                "array" [{"key"   "something"
                                          "'key'" 0}
                                         {"key"   {"key" "russian dolls"}
                                          "'key'" {"'key'" 99}}]}
                      "key"    "top"
                      "'key'"  42}
                     "$..'key'")))
    (is (parse-failed? ;; No consensus, JS result
         (get-values {"some.key"   42
                      "some"       {"key" "value"}
                      "'some.key'" 43}
                 "$.'some.key'")))
    (is (parse-failed? ;; No consensus, JS result
         (get-values {" a" 1 "a" 2 " a " 3 "" 4} "$. a")))
    ;; Wildcard
    (is (= ["string" 42 {"key" "value"} [0 1]]
           (get-values ["string" 42 {"key" "value"} [0 1]] "$.*")))
    (is (= [] (get-values [] "$.*")))
    (is (= [] (get-values {} "$.*")))
    (is (= ["string" 42 {"key" "value"} [0 1]]
           (get-values {"some"   "string"
                        "int"    42
                        "object" {"key" "value"}
                        "array"  [0 1]}
                       "$.*")))
    (is (= [42] (get-values [{"bar" [42]}] "$.*.bar.*")))
    (is (= [1 2 3 4 5 6] (get-values [[1 2 3] [4 5 6]] "$.*.*")))
    ;; Original order: ["string" "value" 0 1 [0 1] {"complex" "string" "primitives" [0 1]}]
    (is (= ["value" {"complex" "string", "primitives" [0 1]} "string" [0 1] 0 1]
           (get-values {"key"         "value"
                        "another key" {"complex"    "string"
                                       "primitives" [0 1]}}
                       "$..*")))
    (is (= [40 nil 42] ;; Original order: [40 42 nil]
           (get-values [40 nil 42] "$..*")))
    (is (= []
           (get-values 42 "$..*")))
    (is (parse-failed?
         (get-values {"a" 1 "$a" 2} "$a")))
    (is (parse-failed?
         (get-values {"key" "value"} ".key")))
    (is (parse-failed?
         (get-values {"key" "value"} "key")))))

(deftest get-at-test-bracket
  (testing "get-at function on bracket notation"
    ;; String array subscripts
    (is (= ["value"]
           (get-values {"key" "value"} "$['key']")))
    (is (= []
           (get-values {"key" "value"} "$['missing']")))
    (is (= [nil]
           (get-values {"key" "value"} "$['missing']" {:return-missing? true})))
    ;; Original order: ["deepest" "first nested" "first" "more" {"nested" ["deepest" "second"]}]
    (is (= ["first" "first nested" {"nested" ["deepest" "second"]} "deepest" "more"]
           (get-values ["first" {"key" ["first nested"
                                        {"more" [{"nested" ["deepest" "second"]}
                                                 ["more" "values"]]}]}]
                       "$..[0]")))
    (is (= []
           (get-values {"ü" 42} "$['ü']")))
    (is (= [nil]
           (get-values {"ü" 42} "$['ü']" {:return-missing? true})))
    (is (= ["42"]
           (get-values {"one"      {"key" "value"}
                        "two"      {"some" "more"
                                    "key"  "other value"}
                        "two.some" "42"}
                       "$['two.some']")))
    (is (= ["value"]
           (get-values {"key" "value"} "$[\"key\"]")))
    (is (parse-failed?
         (get-values {""   42
                      "''" 123
                      "\"\"" 222} "$[]")))
    (is (= [42]
           (get-values {""   42
                        "''" 123
                        "\"\"" 222} "$['']")))
    (is (= [42]
           (get-values {""   42
                        "''" 123
                        "\"\"" 222} "$[\"\"]")))
    ;; Int array subscripts (indices)
    (is (= []
           (get-values ["one element"] "$[-2]")))
    (is (= [nil]
           (get-values ["one element"] "$[-2]" {:return-missing? true})))
    (is (= ["third"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[2]")))
    (is (= []
           (get-values {"0" "value"} "$[0]")))
    (is (= [nil]
           (get-values {"0" "value"} "$[0]" {:return-missing? true})))
    (is (= []
           (get-values ["one element"] "$[1]")))
    (is (= [nil]
           (get-values ["one element"] "$[1]" {:return-missing? true})))
    (is (= []
           (get-values "Hello World" "$[0]")))
    (is (= [nil]
           (get-values "Hello World" "$[0]" {:return-missing? true})))
    (is (= [3]
           (get-values [[1] [2 3]] "$.*[1]")))
    (is (= [nil 3]
           (get-values [[1] [2 3]] "$.*[1]" {:return-missing? true})))
    (is (= ["third"]
           (get-values ["first" "second" "third"] "$[-1]")))
    (is (= ["first"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[0]")))
    ;; Special characters as array subscripts
    (is (= ["value"]
           (get-values {":"       "value"
                        "another" "entry"} "$[':']")))
    (is (= [42]
           (get-values {"]" 42} "$[']']")))
    (is (= ["value"]
           (get-values {"@"       "value"
                        "another" "entry"}
                       "$['@']")))
    (is (= ["value"]
           (get-values {"."       "value"
                        "another" "entry"}
                       "$['.']")))
    (is (= [1]
           (get-values {"key" 42
                        ".*"  1
                        ""    10}
                       "$['.*']")))
    (is (= ["value"]
           (get-values {"\"" "value" "another" "entry"} "$['\"']")))
    (is (= ["value"]
           (get-values {"0" "value"} "$['0']")))
    (is (= ["value"]
           (get-values {"$" "value" "another" "entry"} "$['$']")))
    (is (= ["value"]
           (get-values {"," "value" "another" "entry"} "$[',']")))
    (is (= ["value"]
           (get-values {"*" "value" "another" "entry"} "$['*']")))
    (is (= []
           (get-values {"another" "entry"} "$['*']")))
    (is (= [2]
           (get-values {" a"    1
                        "a"     2
                        " a "   3
                        "a "    4
                        " 'a' " 5
                        " 'a"   6
                        "a' "   7
                        " \"a\" " 8
                        "\"a\""   9}
                       "$[ 'a' ]")))
    (is (= [1]
           (get-values {"nice" 42
                        "ni.*" 1
                        "mice" 100}
                       "$['ni.*']")))
    ;; In the following examples, we deviate from consensus/common sense
    (is (parse-failed? ;; Consensus returns ["value"]; need extra backslashes
         (get-values {"\\" "value"} "$['\\']")))
    (is (= ["value"]
           (get-values {"\\\\" "value"} "$['\\\\']")))
    (is (= [] ;; No consensus, but should return ["value"]
           (get-values {"'" "value"} "$['\\'']")))
    (is (= ["value"] ;; Works when quote in JSON is escaped too
           (get-values {"\\'" "value"} "$['\\'']")))
    (is (parse-failed? ;; The backslashes are needed
         (get-values {"'" "value"} "$[''']")))
    (is (= [42] ;; No consensus
           (get-values {":@.\"$,*\\'\\\\" 42} "$[':@.\"$,*\\'\\\\']")))
    ;; Wildcard operator
    (is (= ["string" 42 {"key" "value"} [0 1]]
           (get-values ["string" 42 {"key" "value"} [0 1]] "$[*]")))
    (is (= []
           (get-values [] "$[*]")))
    (is (= []
           (get-values {} "$[*]")))
    (is (= [40 nil 42]
           (get-values [40 nil 42] "$[*]")))
    (is (= ["string" 42 {"key" "value"} [0 1]]
           (get-values {"some"   "string"
                        "int"    42
                        "object" {"key" "value"}
                        "array"  [0 1]}
                       "$[*]")))
    (is (= [1 2 "a" "b"]
           (get-values [[1 2] ["a" "b"] [0, 0]] "$[0:2][*]")))
    (is (= ["ey" "bee"]
           (get-values [{"key" "ey"} {"key" "bee"} {"key" "see"}] "$[0:2].key")))
    (is (= [42]
           (get-values [{"bar" [42]}] "$[*].bar[*]")))
    ;; Original order ["string" "value" 0 1 [0 1] {"complex" "string" "primitives" [0 1]}]
    (is (= ["value" {"complex" "string" "primitives" [0 1]} "string" [0 1] 0 1]
           (get-values {"key"         "value"
                        "another key" {"complex"    "string"
                                       "primitives" [0 1]}}
                       "$..[*]")))
    ;; Pathological json-path strings
    ;; In cases with no consensus, follow JS jsonpath library
    (is (parse-failed?
         (get-values {"single'quote" "value"} "$['single'quote']")))
    (is (parse-failed?
         (get-values {"one"        {"key" "value"}
                      "two"        {"some" "more"
                                    "key"  "other value"}
                      "two.some"   "42"
                      "two'.'some" "43"}
                     "$['two'.'some']")))
    (is (parse-failed?
         (get-values {"one"      {"key" "value"}
                      "two"      {"some" "more"
                                  "key"  "other value"}
                      "two.some" "42"}
                     "$[two.some]")))
    (is (parse-failed?
         (get-values {"key" "value"} "$[key]")))
    (is (parse-failed?
         (get-values {"key"   "value"
                      "other" {"key" [{"key" 42}]}}
                     "$.['key']")))
    (is (parse-failed?
         (get-values {"key"   "value"
                      "other" {"key" [{"key" 42}]}}
                     "$.[\"key\"]")))
    (is (parse-failed?
         (get-values {"key"   "value"
                      "other" {"key" [{"key" 42}]}}
                     "$.[key]")))))

(deftest get-at-test-union
  (testing "get-at function on union operator"
    (is (= ["first" "second"]
           (get-values ["first" "second" "third"] "$[0,1]")))
    (is (= ["a" "a"]
           (get-values ["a"] "$[0,0]")))
    (is (= ["a"]
           (get-values ["a"] "$[0,0]" {:return-duplicates? false})))
    (is (= [1 1] ;; no consensus, but this is what most implementations return
           (get-values {"a" 1} "$['a','a']")))
    (is (= [1]
           (get-values {"a" 1} "$['a','a']" {:return-duplicates? false})))
    (is (= ["value" "entry"]
           (get-values {"key" "value" "another" "entry"} "$['key','another']")))
    (is (= ["value"]
           (get-values {"key" "value" "another" "entry"} "$['missing','key']")))
    (is (= [nil "value"]
           (get-values {"key" "value" "another" "entry"}
                       "$['missing','key']"
                       {:return-missing? true})))
    (is (= ["cc1" "dd1" "cc2" "dd2"]
           (get-values [{"c" "cc1" "d" "dd1" "e" "ee1"}
                        {"c" "cc2" "d" "dd2" "e" "ee2"}]
                       "$[:]['c','d']")))
    (is (= ["cc1" "dd1"]
           (get-values [{"c" "cc1" "d" "dd1" "e" "ee1"}
                        {"c" "cc2" "d" "dd2" "e" "ee2"}]
                       "$[0]['c','d']")))
    (is (= ["cc1" "dd1" "cc2" "dd2"]
           (get-values [{"c" "cc1" "d" "dd1" "e" "ee1"}
                        {"c" "cc2" "d" "dd2" "e" "ee2"}]
                       "$.*['c','d']")))
    ;; Original order: ["cc1" "cc2" "cc3" "cc5" "dd1" "dd2" "dd4"]
    (is (= ["cc1" "dd1" "cc2" "dd2" "cc3" "dd4" "cc5"]
           (get-values [{"c" "cc1" "d" "dd1" "e" "ee1"}
                        {"c" "cc2" "child" {"d" "dd2"}}
                        {"c" "cc3"}
                        {"d" "dd4"}
                        {"child" {"c" "cc5"}}]
                       "$..['c','d']")))
    (is (= [5 2]
           (get-values [1 2 3 4 5] "$[4,1]")))
    (is (= ["string" "string" nil true false false "string" 5.4]
           (get-values {"a" ["string" nil true]
                        "b" [false "string" 5.4]}
                       "$.*[0,:5]")))
    (is (= ["value" "other value"]
           (get-values {"one"   {"key" "value"}
                        "two"   {"k" "v"}
                        "three" {"some" "more"
                                 "key"  "other value"}}
                       "$['one','three'].key")))
    (is (= [2 3 5] ;; no consensus
           (get-values [1 2 3 4 5] "$[1:3,4]")))
    (is (= ["first" "second"]
           (get-values ["first" "second" "third"] "$[ 0 , 1 ]")))
    (is (parse-failed?
         (get-values ["first" "second" "third" "forth" "fifth"] "$[*,1]")))))

(deftest get-at-test-array-slice
  (testing "get-at function on array slicing"
    ;; Array slice
    (is (= ["second" "third"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[1:3]")))
    (is (= ["first" "second" "third" "fourth" "fifth"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[0:5]")))
    (is (= []
           (get-values ["first" "second" "third"] "$[7:10]")))
    (is (= []
           (get-values ["first" "second" "third"] "$[7:10]" {:return-missing? true})))
    (is (= []
           (get-values {":"    42
                        "more" "string"
                        "a"    1
                        "b"    2
                        "c"    3
                        "1:3"  "nice"}
                       "$[1:3]")))
    (is (= [nil] ;; Returns one nil because JSON is a map, not vector
           (get-values {":"    42
                        "more" "string"
                        "a"    1
                        "b"    2
                        "c"    3
                        "1:3"  "nice"}
                       "$[1:3]"
                       {:return-missing? true})))
    (is (= ["second" "third"]
           (get-values ["first" "second" "third"] "$[1:10]")))
    (is (= ["second" "third"] ;; Slicing excludes out-of-bounds values
           (get-values ["first" "second" "third"] "$[1:10]" {:return-missing? true})))
    ;; Array slice on large numbers
    (is (= ["third" "fourth" "fifth"]
           (get-values ["first" "second" "third" "fourth" "fifth"]
                       "$[2:113667776004]")))
    (is (= ["third" "second" "first"] ;; no consensus, JS jsonpath output
           (get-values ["first" "second" "third" "fourth" "fifth"]
                       "$[2:-113667776004:-1]")))
    (is (= ["first" "second"]
           (get-values ["first" "second" "third" "fourth" "fifth"]
                       "$[-113667776004:2]")))
    (is (= ["fifth" "fourth"] ;; no consensus, but this is a common output
           (get-values ["first" "second" "third" "fourth" "fifth"]
                       "$[113667776004:2:-1]")))
    ;; Array slice w/ negative bounds
    (is (= []
           (get-values [2 "a" 4 5 100 "nice"] "$[-4:-5]")))
    (is (= []
           (get-values [2 "a" 4 5 100 "nice"] "$[-4:-4]")))
    (is (= [4] 
           (get-values [2 "a" 4 5 100 "nice"] "$[-4:-3]")))
    (is (= []
           (get-values [2 "a" 4 5 100 "nice"] "$[-4:2]")))
    (is (= [4] 
           (get-values [2 "a" 4 5 100 "nice"] "$[-4:3]")))
    ;; Array slice w/ negative step size
    ;; None have a consensus; follow Python array slicing output
    (is (= ["fourth" "second"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[3:0:-2]")))
    (is (= ["fifth"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[7:3:-1]")))
    (is (= []
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[0:3:-2]")))
    ;; Array slicing w/ steps and w/o bounds
    (is (= ["fifth" "third" "first"] ;; no consensus, follow Python
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[::-2]")))
    (is (= ["second" "third" "fourth" "fifth"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[1:]")))
    (is (= ["fourth" "third" "second" "first"] ;; no consensus, follow Python
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[3::-1]")))
    (is (= ["first" "second"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[:2]")))
    (is (= ["first" "second"]
           (get-values ["first" "second"] "$[:]")))
    (is (= []
           (get-values {":" 42 "more" "string"} "$[:]")))
    (is (= ["first" "second"]
           (get-values ["first" "second"] "$[::]")))
    (is (= ["fifth" "fourth"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[:2:-1]")))
    (is (= []
           (get-values [2 "a" 4 5 100 "nice"] "$[3:-4]")))
    (is (= []
           (get-values [2 "a" 4 5 100 "nice"] "$[3:-3]")))
    (is (= [5] 
           (get-values [2 "a" 4 5 100 "nice"] "$[3:-2]")))
    (is (= []
           (get-values ["first" "second" "third" "forth"] "$[2:1]")))
    (is (= []
           (get-values ["first" "second"] "$[0:0]")))
    (is (= ["first"]
           (get-values ["first" "second"] "$[0:1]")))
    (is (= ["third"]
           (get-values ["first" "second" "third"] "$[-1:]")))
    (is (= ["second" "third"]
           (get-values ["first" "second" "third"] "$[-2:]")))
    (is (= ["first" "second" "third"] 
           (get-values ["first" "second" "third"] "$[-4:]")))
    (is (= ["first" "third"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[0:3:2]")))
    (is (= ["first" "second" "third"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[0:3:0]")))
    (is (= ["first" "second" "third"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[0:3:1]")))
    (is (= [10 20] 
           (get-values [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25]
                   "$[010:024:010]")))
    (is (= ["first" "third"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[0:4:2]")))
    (is (= ["second" "third"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[1:3]")))
    (is (= ["first" "third" "fifth"]
           (get-values ["first" "second" "third" "fourth" "fifth"] "$[::2]")))))

(deftest get-at-test-misc
  (testing "get-at function on misc jsonpath strings"
    (is (parse-failed?
         (get-values {"a" 42 "" 21} "")))
    (is (parse-failed?
         (get-values {"key" 1 "some" 2 "more" 3} "$(key,more)")))
    (is (parse-failed?
         (get-values [{"a" {"b" "c"}} [0 1]] "$..")))
    (is (parse-failed?
         (get-values {"some key" "value" "key" {"complex" "string" "primitives" [0 1]}}
                     "$.key..")))
    (is (= [{"key"         "value"
             "another key" {"complex" ["a" 1]}}] 
           (get-values {"key"         "value"
                        "another key" {"complex" ["a" 1]}}
                       "$")))
    (is (= [42]
           (get-values 42 "$")))
    (is (= [false]
           (get-values false "$")))
    (is (= [true]
           (get-values true "$")))))
