(ns com.yetanalytics.pathetic.json-path-test
  (:require [clojure.test :refer [deftest testing is are]]
            #_[clojure.spec.gen.alpha :as sgen]
            [clojure.test.check]
            [clojure.test.check.properties]
            [clojure.spec.test.alpha :as stest]
            [com.yetanalytics.pathetic.json-path
             :refer [path->string parse parse-first is-parse-failure?]
             :as json-path]))

(deftest path->string-test
  (testing "Converting parsed paths back to strings"
    (is (= "$[0,1,foo,0:5:1]..[*]"
           (path->string
            [[0 1 "foo" {:start 0 :end 5 :step 1}] '.. '*])))))

(deftest parse-test-0
  (testing "Original JSONPath tests"
    (are [path v] (= v (parse-first path))
      ;; Selections from original Goessner website
      "$.store.book[*].author" [["store"] ["book"] '* ["author"]]
      "$..author"              ['.. ["author"]]
      "$.store.*"              [["store"] '*]
      "$.store..price"         [["store"] '.. ["price"]]
      "$..book[2]"             ['.. ["book"] [2]]
      "$..book[-1:]"           ['.. ["book"] [{:start -1 :end :vec-higher :step 1}]]
      "$..book[0,1]"           ['.. ["book"] [0 1]]
      "$..book[:2]"            ['.. ["book"] [{:start :vec-lower :end 2 :step 1}]]
      "$..*"                   ['.. '*]
      ;; Selections from cmi5
      "$.context.contextActivities.grouping[*]"
      [["context"] ["contextActivities"] ["grouping"] '*]
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/sessionid']"
      [["context"] ["extensions"] ["https://w3id.org/xapi/cmi5/context/extensions/sessionid"]])))

(deftest parse-test-1
  (testing "JSONPath strings with dot notation"
    ;; Successes
    (are [path v] (= v (parse-first path))
      "$"            [] ; Jayway only
      "$.store"      [["store"]]
      "$.store.book" [["store"] ["book"]]
      "$..book"      ['.. ["book"]]
      "$.*"          ['*]
      "$.*.*"        ['* '*]
      "$.store.*"    [["store"] '*]
      "$.*.book"     ['* ["book"]]
      "$..*"         ['.. '*])
    ;; Failures
    (are [path] (is-parse-failure? (parse-first path))
      ""
      ".store"
      "$."
      "$.."
      "$store"
      "$.store,expensive"
      "$.store, expensive"
      "$.store expensive"
      "$.store[expensive"
      "$. store"
      "$.**" ; Supported by Jayway, not supported by everyone else
      "$.store*"
      "$.store*io"
      "what the pineapple")))

(deftest parse-test-2
  (testing "JSONPath strings with bracket notation"
    ;; Successes
    (are [path v] (= v (parse-first path))
      "$['store']"              [["store"]]
      "$['store shop']"         [["store shop"]]
      "$['store']['book']"      [["store"] ["book"]]
      "$[   ' store '   ]"      [[" store "]]; Jayway supports
      "$.store['book']"         [["store"] ["book"]]
      "$['store'].book"         [["store"] ["book"]]
      "$..['book']"             ['.. ["book"]]
      "$..['author']"           ['.. ["author"]]
      "$[*]"                    ['*]
      "$[*][*]"                 ['* '*]
      "$[    *    ]"            ['*]
      "$['*']"                  [["*"]] ; Is a string, not a wildcard
      "$['store'][*]"           [["store"] '*]
      "$['store']['book']['*']" [["store"] ["book"] ["*"]]
      "$[*]['books']"           ['* ["books"]]
      "$['store\\'s']"          [["store\\'s"]]; Jayway supports escaped chars
      "$[\"\\\"store\\\"\"]"    [["\\\"store\\\""]]
      "$['store,expensive']"    [["store,expensive"]]
      "$['store']['book*']"     [["store"] ["book*"]]; All chars except ',' and ''' supported
      "$['store','expensive']"  [["store" "expensive"]]
      "$['store', 'expensive']" [["store" "expensive"]]
      "$['store',     'books']" [["store" "books"]])
    ;; Failures
    (are [path] (is-parse-failure? (parse-first path))
      "['store']"
      "$[]"
      "$['store"
      "$[**]"
      "$[store]"             ; Jayway does not support
      "$['store'] ['book']"
      "$['store']book"
      "$['store's']"
      "$['store''expensive']"
      "$['store' 'expensive']")))

(deftest parse-test-3
  (testing "JSONPath strings with arrays"
    ;; Successes
    (are [path v] (= v (parse-first path))
      "$[0]"                            [[0]]
      "$..[0]"                          ['.. [0]]
      "$..book[0]"                      ['.. ["book"] [0]]
      "$..book[1234]"                   ['.. ["book"] [1234]]
      "$.store.book[0]"                 [["store"] ["book"] [0]]
      "$.store.book[0, 1]"              [["store"] ["book"] [0 1]]
      "$.store.book[*]"                 [["store"] ["book"] '*]
      "$.store['book'].*"               [["store"] ["book"] '*]
      "$['store']['book'][0]"           [["store"] ["book"] [0]]
      "$['store']['book'][0][0]"        [["store"] ["book"] [0] [0]]
      "$['store']['book'][0 , 1]"       [["store"] ["book"] [0 1]]
      "$['store']['book'][*]"           [["store"] ["book"] '*]
      "$['store']['book'][0]['author']" [["store"] ["book"] [0] ["author"]]
      "$['store']['book']['*'][0]"      [["store"] ["book"] ["*"] [0]]
      "$.book[-1]"                      [["book"] [-1]]
      ;; Array slicing
      "$.book[0:6:2]"   [["book"] [{:start 0 :end 6 :step 2}]]
      "$[:]"            [[{:start :vec-lower :end :vec-higher :step 1}]]
      "$[::]"           [[{:start :vec-lower :end :vec-higher :step 1}]]
      "$[1:]"           [[{:start 1 :end :vec-higher :step 1}]]
      "$[-1:]"          [[{:start -1 :end :vec-higher :step 1}]]
      "$[:1]"           [[{:start :vec-lower :end 1 :step 1}]]
      "$[:-2]"          [[{:start :vec-lower :end -2 :step 1}]]
      "$[0:2]"          [[{:start 0 :end 2 :step 1}]]
      "$[0:3:]"         [[{:start 0 :end 3 :step 1}]]
      "$[::2]"          [[{:start :vec-lower :end :vec-higher :step 2}]]
      "$[::-3]"         [[{:start :vec-higher :end :vec-lower :step -3}]]
      "$[1::4]"         [[{:start 1 :end :vec-higher :step 4}]]
      "$[1::-4]"        [[{:start 1 :end :vec-lower :step -4}]]
      "$[:10:3]"        [[{:start :vec-lower :end 10 :step 3}]]
      "$[:1:-3]"        [[{:start :vec-higher :end 1 :step -3}]]
      "$[0:4:2,1]"      [[{:start 0 :end 4 :step 2} 1]]
      "$[0:5:1]..[*]"   [[{:start 0 :end 5 :step 1}] '.. '*])
    ;; Failure
    (are [path] (is-parse-failure? (parse-first path))
      "$.store.book[[0]"
      "$.store.book[0]]"
      "$..book[?(@isbn)]" ; Filter fns not supported in xAPI Profile spec
      "$..book[l33t]")))

(deftest parse-test-4
  (testing "JSONPath strings that have been joined with '|'"
    (are [paths v] (= v (parse paths))
      "$.store.book|$.results.extensions"
      [[["store"] ["book"]] [["results"] ["extensions"]]]
      "$.store.book  |$.results.extensions"
      [[["store"] ["book"]] [["results"] ["extensions"]]]
      "$.store.book|  $.results.extensions"
      [[["store"] ["book"]] [["results"] ["extensions"]]]
      "$.store.book | $.results.extensions"
      [[["store"] ["book"]] [["results"] ["extensions"]]]
      "$['store']['book']|$['results']['extensions']"
      [[["store"] ["book"]] [["results"] ["extensions"]]]
      "$['store']['book'] | $['results']['extensions']"
      [[["store"] ["book"]] [["results"] ["extensions"]]]
      "$['store']['book'][0] | $.results.extensions['https://foo.org']"
      [[["store"] ["book"] [0]] [["results"] ["extensions"] ["https://foo.org"]]]
      "$['store   |$.io']"
      [[["store   |$.io"]]]
      "$['store|$.io']"
      [[["store|$.io"]]]
      "$.store[*] | $.results['b|ah']"
      [[["store"] '*] [["results"] ["b|ah"]]])
    (are [paths] (is-parse-failure? (parse paths))
      "$.store.book $.results.extension"
      "$.store.book | what the pineapple"
      "$['store|$['io']]")))

;; Note: path-seqs and speculative-path-seqs are tested indirectly in
;; pathetic-test

(deftest gen-tests
  (testing "Generative tests"
    (stest/check `[json-path/is-parse-failure?
                   json-path/test-strict-path
                   json-path/path->string
                   json-path/parse
                   json-path/parse-first]
                 {:clojure.spec.test.check/opts
                  {:num-tests #?(:clj 100 :cljs 10)
                   :seed (rand-int 2000000000)}})))
