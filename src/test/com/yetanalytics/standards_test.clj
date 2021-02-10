(ns com.yetanalytics.standards-test
  (:require [clojure.test :refer [deftest testing is]]
            [com.yetanalytics.pathetic :refer [get-at]]
            #_[com.yetanalytics.standards-test :refer [parse-failed?]]))

;; This test suite is designed to compare Pathetic against the JSONPath
;; examples given by Christoph Burgmer's test suite at:
;; https://cburgmer.github.io/json-path-comparison/
;; 
;; Unless otherwise specified, Pathetic attempts to follow the consensus given
;; by different JSONPath implementations. If not consensus exists, then
;; Pathetic will default to throwing a parse failure, if that is the reason
;; for lack of consensus.
;; 
;; Note: We do not test script/filter expressions as we do not suppor them.

(defmacro parse-failed? [form]
  `(try ~form
        (catch Exception e# (= "Cannot parse JSONPath string"
                               (ex-message e#)))))

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
           (get-at {"key" "value"} "$.key")))
    (is (= [] 
           (get-at [0 1] "$.key")))
    (is (= [nil] 
           (get-at [0 1] "$.key" :return-missing? true)))
    (is (= [["first" "second"]]
           (get-at {"key" ["first" "second"]} "$.key")))
    (is (= [] 
           (get-at [{"id" 2}] "$.key")))
    (is (= [nil] 
           (get-at [{"id" 2}] "$.key" :return-missing? true)))
    (is (= [{}]
           (get-at {"key" {}} "$.key")))
    (is (= [nil]
           (get-at {"key" nil} "$.key")))
    (is (= [] 
           (get-at {"key" "value"} "$.missing")))
    (is (= [nil]
           (get-at {"key" "value"} "$.missing" :return-missing? true)))
    (is (= ["ey" "bee"]
           (get-at [{"key" "ey"} {"key" "bee"} {"key" "see"}] "$[0:2].key")))
    #_(is (= [42 200 500] ;; Original was [200 42 500]
           (get-at {"k"   [{"key" "some value"}, {"key" 42}]
                    "kk"  [[{"key" 100} {"key" 200} {"key" 300}]
                           [{"key" 400} {"key" 500} {"key" 600}]]
                    "key" [0 1]}
                   "$..[1].key")))
    (is (= [1 1] 
           (get-at [{"a" 1} {"a" 1}] "$[*].a")))
    (is (= [1]
           (get-at [{"a" 1} {"a" 1}] "$[*].a" :return-duplicates? false)))
    (is (= [1]
           (get-at [{"a" 1}] "$[*].a")))
    (is (= [1] 
           (get-at [{"a" 1} {"b" 1}] "$[*].a")))
    (is (= [1 nil]
           (get-at [{"a" 1} {"b" 1}] "$[*].a" :return-missing? true)))
    ;; Original order: ["russian dolls" "something" "top" "value" {"key" "russian dolls"}]
    #_(is (= ["top" "value" "something" {"key" "russian dolls"} "russian dolls"]
           (get-at {"object" {"key"   "value"
                              "array" [{"key" "something"}
                                       {"key" {"key" "russian dolls"}}]}
                    "key"    "top"}
                   "$..key")))
    ;; Original order: [12.99 19.95 22.99 8.95 8.99]
    #_(is (= [8.95 12.99 8.99 22.99 19.95]
           (get-at store-example "$.store..price")))
    #_(is (= [nil 8.95 12.99 8.99 22.99 19.95]
           (get-at store-example "$.store..price" :return-missing? true)))
    (is (= ["ey" "see"]
           (get-at [{"key" "ey"} {"key" "bee"} {"key" "see"}] "$[0,2].key")))
    (is (= ["other value" "value"] ;; Original order: ["value" "other value"]
           (get-at {"one"   {"key" "value"}
                    "two"   {"k" "v"}
                    "three" {"some" "more"
                             "key"  "other value"}}
                   "$['one','three'].key")))
    (is (= ["value"]
           (get-at {"key-dash" "value"} "$.key-dash")))
    (is (parse-failed?
         (get-at {"key" "value" "\"key\"" 42} "$.\"key\"")))
    (is (parse-failed?
         (get-at {"key" 42 "" 9001 "''" "nice"} "$.")))
    ;; Keyword keys
    (is (= ["value"] (get-at {"in" "value"} "$.in")))
    (is (= ["value"] (get-at {"length" "value"} "$.length")))
    (is (= [] (get-at [4 5 6] "$.length")))
    (is (= ["value"] (get-at {"nil" "value"} "$.nil")))
    (is (= ["value"] (get-at {"true" "value"} "$.true")))
    (is (parse-failed?
         (get-at {"$" "value"} "$.$")))
    ;; Consensus for non-ASCII keys is ["value"], but we instead go with the JS
    ;; jsonpath lib and throw a parse error
    (is (parse-failed?
         (get-at {"上下" "value"} "$.上下")))
    (is (parse-failed?
         (get-at {"上下" "value"} "$.\u4E0A\u4E0B")))
    (is (= [] ;; No consensus, we treat "2" as an identifier
           (get-at ["first" "second" "third" "fourth" "fifth"] "$.2")))
    (is (= ["second"] 
           (get-at {"a" "first" "2" "second" "c" "third"} "$.2")))
    (is (= [] ;; No consensus
           (get-at ["first" "second" "third" "fourth" "fifth"] "$.-1")))
    (is (parse-failed? ;; No consensus, JS result
         (get-at {"key" "value"  "'key'" 42} "$.'key'")))
    #_(is (parse-failed? ;; No consensus, JS result
         (get-at {"object" {"key"   "value"
                            "'key'" 100
                            "array" [{"key"   "something"
                                      "'key'" 0}
                                     {"key"   {"key" "russian dolls"}
                                      "'key'" {"'key'" 99}}]}
                    "key"    "top"
                    "'key'"  42}
                   "$..'key'")))
    (is (parse-failed? ;; No consensus, JS result
         (get-at {"some.key"   42
                  "some"       {"key" "value"}
                  "'some.key'" 43}
                 "$.'some.key'")))
    (is (parse-failed? ;; No consensus, JS result
         (get-at {" a" 1 "a" 2 " a " 3 "" 4} "$. a")))
    ;; Wildcard
    (is (= ["string" 42 {"key" "value"} [0 1]]
           (get-at ["string" 42 {"key" "value"} [0 1]] "$.*")))
    (is (= [] (get-at [] "$.*")))
    (is (= [] (get-at {} "$.*")))
    (is (= ["string" 42 {"key" "value"} [0 1]]
           (get-at {"some"   "string"
                    "int"    42
                    "object" {"key" "value"}
                    "array"  [0 1]}
                   "$.*")))
    (is (= [42] (get-at [{"bar" [42]}] "$.*.bar.*")))
    (is (= [1 2 3 4 5 6] (get-at [[1 2 3] [4 5 6]] "$.*.*")))
    ;; ["value" {"complex" "string", "primitives" [0 1]} nil "string" [0 1] 0 1]
    #_(is (= ["string" "value" 0 1 [0 1] {"complex" "string" "primitives" [0 1]}]
           (get-at {"key"         "value"
                    "another key" {"complex"    "string"
                                   "primitives" [0 1]}}
                   "$..*")))
    #_(is (= [40 42 nil] ;; [40 nil 42]
           (get-at [40 nil 42] "$..*")))
    #_(is (= [nil]
           (get-at 42 "$..*")))
    (is (parse-failed?
         (get-at {"a" 1 "$a" 2} "$a")))
    (is (parse-failed?
         (get-at {"key" "value"} ".key")))
    (is (parse-failed?
         (get-at {"key" "value"} "key")))))

(deftest get-at-test-bracket
  (testing "get-at function on bracket notation"
    ;; String array subscripts
    (is (= ["value"]
           (get-at {"key" "value"} "$['key']")))
    (is (= [] ;; [nil]
           (get-at {"key" "value"} "$['missing']")))
    ;; ["first" nil "first nested" {"nested" ["deepest" "second"]} "deepest" "more"]
    #_(is (= ["deepest" "first nested" "first" "more" {"nested" ["deepest" "second"]}]
           (get-at ["first" {"key" ["first nested"
                                    {"more" [{"nested" ["deepest" "second"]}
                                             ["more" "values"]]}]}]
                   "$..[0]")))
    (is (= [] 
           (get-at {"ü" 42} "$['ü']")))
    (is (= [nil] 
           (get-at {"ü" 42} "$['ü']" :return-missing? true)))
    (is (= ["42"]
           (get-at {"one" {"key" "value"}
                    "two" {"some" "more" "key" "other value"}
                    "two.some" "42"}
                   "$['two.some']")))
    (is (= ["value"]
           (get-at {"key" "value"} "$[\"key\"]")))
    (is (parse-failed?
         (get-at {"" 42, "''" 123 "\"\"" 222} "$[]")))
    (is (= [42]
           (get-at {"" 42, "''" 123 "\"\"" 222} "$['']")))
    (is (= [42]
           (get-at {"" 42, "''" 123 "\"\"" 222} "$[\"\"]")))
    ;; Int array subscripts (indices)
    (is (= [] 
           (get-at ["one element"] "$[-2]")))
    (is (= [nil] 
           (get-at ["one element"] "$[-2]" :return-missing? true)))
    (is (= ["third"]
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[2]")))
    (is (= [] 
           (get-at {"0" "value"} "$[0]")))
    (is (= [nil] 
           (get-at {"0" "value"} "$[0]" :return-missing? true)))
    (is (= [] 
           (get-at ["one element"] "$[1]")))
    (is (= [nil] 
           (get-at ["one element"] "$[1]" :return-missing? true)))
    #_(is (= [] ;; [nil]
           (get-at "Hello World" "$[0]")))
    (is (= [3] 
           (get-at [[1] [2 3]] "$.*[1]")))
    (is (= [nil 3]
           (get-at [[1] [2 3]] "$.*[1]" :return-missing? true)))
    #_(is (= ["third"] ;; []
           (get-at ["first" "second" "third"] "$[-1]")))
    (is (= ["first"]
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[0]")))
    ;; Special characters as array subscripts
    (is (= ["value"]
           (get-at {":" "value" "another" "entry"} "$[':']")))
    (is (= [42]
           (get-at {"]" 42} "$[']']")))
    (is (= ["value"]
           (get-at {"@" "value" "another" "entry"} "$['@']")))
    (is (= ["value"]
           (get-at {"." "value" "another" "entry"} "$['.']")))
    (is (= [1]
           (get-at {"key" 42 ".*" 1 "" 10} "$['.*']")))
    (is (= ["value"]
           (get-at {"\"" "value" "another" "entry"} "$['\"']")))
    ;; (is (= ["value"] ;; []
    ;;        (get-at {"\\" "value"} "$['\\']")))
    ;; (is (= ["value"] ;; Throws exception, NO CONSENSUS 
    ;;        (get-at {"'" "value"} "$['\'']")))
    (is (= ["value"]
           (get-at {"0" "value"} "$['0']")))
    (is (= ["value"]
           (get-at {"$" "value" "another" "entry"} "$['$']")))
    ;; (is (= [] ;; NO CONSENSUS
    ;;        (get-at {":@.\"$,*'\\" 42} "$[':@.\"$,*\'\\']")))
    (is (= ["value"]
           (get-at {"," "value" "another" "entry"} "$[',']")))
    (is (= ["value"]
           (get-at {"*" "value" "another" "entry"} "$['*']")))
    (is (= []
           (get-at {"another" "entry"} "$['*']")))
    (is (= [2]
           (get-at {" a"    1
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
           (get-at {"nice" 42 "ni.*" 1 "mice" 100} "$['ni.*']")))
    ;; Wildcard operator
    (is (= ["string" 42 {"key" "value"} [0 1]]
           (get-at ["string" 42 {"key" "value"} [0 1]] "$[*]")))
    (is (= []
           (get-at [] "$[*]")))
    (is (= []
           (get-at {} "$[*]")))
    (is (= [40 nil 42]
           (get-at [40 nil 42] "$[*]")))
    (is (= ["string" 42 {"key" "value"} [0 1]]
           (get-at {"some"   "string"
                    "int"    42
                    "object" {"key" "value"}
                    "array"  [0 1]}
                   "$[*]")))
    (is (= [1 2 "a" "b"]
           (get-at [[1 2] ["a" "b"] [0, 0]] "$[0:2][*]")))
    (is (= [42]
           (get-at [{"bar" [42]}] "$[*].bar[*]")))
    ;; ["value" {"complex" "string", "primitives" [0 1]} nil "string" [0 1] 0 1]
    #_(is (= ["string" "value" 0 1 [0 1] {"complex" "string" "primitives" [0 1]}]
           (get-at {"key"         "value"
                    "another key" {"complex"    "string"
                                   "primitives" [0 1]}}
                   "$..[*]")))
    ;; Pathological json-path strings
    ;; In cases with no consensus, follow JS jsonpath library
    (is (parse-failed?
         (get-at {"single'quote" "value"} "$['single'quote']")))
    (is (parse-failed?
         (get-at {"one"        {"key" "value"}
                  "two"        {"some" "more"
                                "key"  "other value"}
                  "two.some"   "42"
                  "two'.'some" "43"}
                 "$['two'.'some']")))
    (is (parse-failed?
         (get-at {"one"      {"key" "value"}
                  "two"      {"some" "more"
                              "key"  "other value"}
                  "two.some" "42"}
                 "$[two.some]")))
    (is (parse-failed?
           (get-at {"key" "value"} "$[key]")))
    (is (parse-failed?
         (get-at {"key"   "value"
                  "other" {"key" [{"key" 42}]}}
                 "$.['key']")))
    (is (parse-failed?
           (get-at {"key"   "value"
                    "other" {"key" [{"key" 42}]}}
                   "$.[\"key\"]")))
    (is (parse-failed?
           (get-at {"key"   "value"
                    "other" {"key" [{"key" 42}]}}
                   "$.[key]")))))

(deftest get-at-test-union
  (testing "get-at function on union operator"
    (is (= ["first" "second"]
           (get-at ["first" "second" "third"] "$[0,1]")))
    (is (= ["a" "a"] ;; ["a"]
           (get-at ["a"] "$[0,0]")))
    (is (= [1 1] ;; [1] NO CONSENSUS
           (get-at {"a" 1} "$['a','a']")))
    (is (= ["value" "entry"]
           (get-at {"key" "value" "another" "entry"} "$['key','another']")))
    (is (= ["value"]
           (get-at {"key" "value" "another" "entry"} "$['missing','key']")))
    (is (= [nil "value"]
           (get-at {"key" "value" "another" "entry"}
                   "$['missing','key']"
                   :return-missing? true)))
    (is (= ["cc1" "dd1" "cc2" "dd2"] ;; out of order
           (get-at [{"c" "cc1" "d" "dd1" "e" "ee1"}
                    {"c" "cc2" "d" "dd2" "e" "ee2"}]
                   "$[:]['c','d']")))
    (is (= ["cc1" "dd1"] ;; out of order
           (get-at [{"c" "cc1" "d" "dd1" "e" "ee1"}
                    {"c" "cc2" "d" "dd2" "e" "ee2"}]
                   "$[0]['c','d']")))
    (is (= ["cc1" "dd1" "cc2" "dd2"] ;; out of order
           (get-at [{"c" "cc1" "d" "dd1" "e" "ee1"}
                    {"c" "cc2" "d" "dd2" "e" "ee2"}]
                   "$.*['c','d']")))
    (is (= ["cc1" "cc2" "cc3" "cc5" "dd1" "dd2" "dd4"] ;; [nil "dd1" "cc1" "cc2" "dd2" "cc3" "dd4" "cc5"] NO CONSENSUS
           (get-at [{"c" "cc1" "d" "dd1" "e" "ee1"}
                    {"c" "cc2" "child" {"d" "dd2"}}
                    {"c" "cc3"}
                    {"d" "dd4"}
                    {"child" {"c" "cc5"}}]
                   "$..['c','d']")))
    (is (= [5 2] ;; [2 5]
           (get-at [1 2 3 4 5] "$[4,1]")))
    (is (= ["string" "string" nil true false false "string" 5.4] ;; ["string" nil true false 5.4] NO CONSENSUS
           (get-at {"a" ["string" nil true]
                    "b" [false "string" 5.4]}
                   "$.*[0,:5]")))
    (is (= [2 3 5] ;; [2 5 3] NO CONSENSUS
           (get-at [1 2 3 4 5] "$[1:3,4]")))
    (is (= ["first" "second"]
           (get-at ["first" "second" "third"] "$[ 0 , 1 ]")))
    (is (parse-failed?
         (get-at ["first" "second" "third" "forth" "fifth"] "$[*,1]")))))

;; Skip the large number tests for performance reasons

(deftest get-at-test-array-slice
  (testing "get-at function on array slicing"
    ;; Array slice
    (is (= ["second" "third"]
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[1:3]")))
    (is (= ["first" "second" "third" "forth" "fifth"] ;; out of order
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[0:5]")))
    (is (= [] 
           (get-at ["first" "second" "third"] "$[7:10]")))
    (is (= [nil nil nil]
           (get-at ["first" "second" "third"] "$[7:10]" :return-missing? true)))
    (is (= [] ;; Throws exception
           (get-at {":"    42
                    "more" "string"
                    "a"    1
                    "b"    2
                    "c"    3
                    "1:3"  "nice"}
                   "$[1:3]")))
    (is (= ["second" "third"]
           (get-at ["first" "second" "third"] "$[1:10]")))
    (is (= ["second" "third" nil nil nil nil nil nil nil]
           (get-at ["first" "second" "third"] "$[1:10]" :return-missing? true)))
    ;; Array slice w/ negative numbers
    (is (= []
           (get-at [2 "a" 4 5 100 "nice"] "$[-4:-5]")))
    (is (= []
           (get-at [2 "a" 4 5 100 "nice"] "$[-4:-4]")))
    (is (= [4] ;; []
           (get-at [2 "a" 4 5 100 "nice"] "$[-4:-3]")))
    (is (= [] 
           (get-at [2 "a" 4 5 100 "nice"] "$[-4:2]")))
    (is (= [nil nil]
           (get-at [2 "a" 4 5 100 "nice"] "$[-4:2]" :return-missing? true)))
    (is (= [4] ;; [nil]
           (get-at [2 "a" 4 5 100 "nice"] "$[-4:3]")))
    (is (= [] ;; NO CONSENSUS
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[3:0:-2]")))
    (is (= [] ;; [nil] NO CONSENSUS
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[7:3:-1]")))
    (is (= [] ;; NO CONSENSUS
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[0:3:-2]")))
    ;; Array slicing w/ steps and w/o bounds
    (is (= ["fifth" "third" "first"] ;; [] NO CONSENSUS
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[::-2]")))
    (is (= ["second" "third" "fourth" "fifth"] ;; [... nil]
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[1:]")))
    (is (= ["fourth" "third" "second" "first"] ;; [] NO CONSENSUS
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[3::-1]")))
    (is (= ["first" "second"]
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[:2]")))
    (is (= ["first" "second"]
           (get-at ["first" "second"] "$[:]")))
    (is (= [] ;; Throws exception
           (get-at {":" 42 "more" "string"} "$[:]")))
    (is (= ["first" "second"]
           (get-at ["first" "second"] "$[::]")))
    (is (= ["fifth" "fourth"] ;; [] NO CONSENSUS
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[:2:-1]")))
    (is (= []
           (get-at [2 "a" 4 5 100 "nice"] "$[3:-4]")))
    (is (= []
           (get-at [2 "a" 4 5 100 "nice"] "$[3:-3]")))
    (is (= [5] ;; []
           (get-at [2 "a" 4 5 100 "nice"] "$[3:-2]")))
    (is (= []
           (get-at ["first" "second" "third" "forth"] "$[2:1]")))
    (is (= []
           (get-at ["first" "second"] "$[0:0]")))
    (is (= ["first"]
           (get-at ["first" "second"] "$[0:1]")))
    (is (= ["third"] ;; ["first" "second" nil]
           (get-at ["first" "second" "third"] "$[-1:]")))
    (is (= ["second" "third"] ;; ["first" nil]
           (get-at ["first" "second" "third"] "$[-2:]")))
    (is (= ["first" "second" "third"] ;; [nil]
           (get-at ["first" "second" "third"] "$[-4:]")))
    (is (= ["first" "third"]
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[0:3:2]")))
    (is (= ["first" "second" "third"] ;; ["first"]
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[0:3:0]")))
    (is (= ["first" "second" "third"]
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[0:3:1]")))
    (is (= [10 20] ;; []
           (get-at [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25]
                   "$[010:020:030]")))
    (is (= ["first" "third"]
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[0:4:2]")))
    (is (= ["second" "third"]
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[1:3]")))
    (is (= ["first" "third" "fifth"] ;; ["first" "fifth" nil "third"]
           (get-at ["first" "second" "third" "fourth" "fifth"] "$[::2]")))))

(deftest get-at-test-misc
  (testing "get-at function on misc jsonpath strings"
    (is (parse-failed?
         (get-at {"a" 42 "" 21} "")))
    (is (parse-failed?
         (get-at {"key" 1 "some" 2 "more" 3} "$(key,more)")))
    (is (parse-failed?
         (get-at [{"a" {"b" "c"}} [0 1]] "$..")))
    (is (parse-failed?
         (get-at {"some key" "value" "key" {"complex" "string" "primitives" [0 1]}}
                 "$.key..")))
    (is (= [{"key"         "value"
             "another key" {"complex" ["a" 1]}}] ;; []
           (get-at {"key"         "value"
                    "another key" {"complex" ["a" 1]}}
                   "$")))
    (is (= [42] 
           (get-at 42 "$")))
    (is (= [false] 
           (get-at false "$")))
    (is (= [true] 
           (get-at true "$")))))
