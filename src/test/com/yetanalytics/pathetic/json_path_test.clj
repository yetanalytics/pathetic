(ns com.yetanalytics.pathetic.json-path-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.pathetic.json-path :refer :all]
            [blancas.kern.core :as k]))

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

(def ^:const max-int 9223372036854775807) ; TODO: add cljs version

(deftest parse-test-0
  (testing "Original JSONPath tests"
    (are [path v] (= v (parse-first path))
    ;; Selections from original Goessner website
      "$.store.book[*].author" [#{"store"} #{"book"} '* #{"author"}]
      "$..author"              ['.. #{"author"}]
      "$.store.*"              [#{"store"} '*]
      "$.store..price"         [#{"store"} '.. #{"price"}]
      "$..book[2]"             ['.. #{"book"} #{2}]
      "$..book[-1:]"           ['.. #{"book"} #{{:start -1 :end max-int :step 1 :bounded? false}}]
      "$..book[0,1]"           ['.. #{"book"} #{0 1}]
      "$..book[:2]"            ['.. #{"book"} #{{:start 0 :end 2 :step 1 :bounded? true}}]
      "$..*"                   ['.. '*]
    ;; Selections from cmi5
      "$.context.contextActivities.grouping[*]"
      [#{"context"} #{"contextActivities"} #{"grouping"} '*]
      "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/sessionid']"
      [#{"context"} #{"extensions"} #{"https://w3id.org/xapi/cmi5/context/extensions/sessionid"}])))

(deftest parse-test-1
  (testing "JSONPath strings with dot notation"
    (are [path v] (= v (parse-first path))
      "$"            [] ; Jayway only
      "$.store"      [#{"store"}]
      "$.store.book" [#{"store"} #{"book"}]
      "$..book"      ['.. #{"book"}]
      "$.*"          ['*]
      "$.*.*"        ['* '*]
      "$.store.*"    [#{"store"} '*]
      "$.*.book"     ['* #{"book"}]
      "$..*"         ['.. '*])
    (are [path] (= nil (parse-first path))
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
    (are [path v] (= v (parse-first path))
      "$['store']"              [#{"store"}]
      "$['store shop']"         [#{"store shop"}]
      "$['store']['book']"      [#{"store"} #{"book"}]
      "$[   ' store '   ]"      [#{" store "}]; Jayway supports
      "$.store['book']"         [#{"store"} #{"book"}]
      "$['store'].book"         [#{"store"} #{"book"}]
      "$..['book']"             ['.. #{"book"}]
      "$..['author']"           ['.. #{"author"}]
      "$[*]"                    ['*]
      "$[*][*]"                 ['* '*]
      "$[    *    ]"            ['*]
      "$['*']"                  [#{"*"}] ; Is a string, not a wildcard
      "$['store'][*]"           [#{"store"} '*]
      "$['store']['book']['*']" [#{"store"} #{"book"} #{"*"}]
      "$[*]['books']"           ['* #{"books"}]
     ;;  "$['store\\'s']"          [#{"store\\'s"}]; Jayway supports escaped chars
      "$['store,expensive']"     [#{"store,expensive"}]
      "$['store']['book*']"     [#{"store"} #{"book*"}]; All chars except ',' and ''' supported
      "$['store','expensive']"  [#{"expensive", "store"}]
      "$['store', 'expensive']" [#{"expensive", "store"}]
      "$['store',     'books']" [#{"books", "store"}])
    (are [path] (= nil (parse-first path))
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
    (are [path v] (= v (parse-first path))
      "$..[0]"                          ['.. #{0}]
      "$..book[0]"                      ['.. #{"book"} #{0}]
      "$..book[1234]"                   ['.. #{"book"} #{1234}]
      "$.store.book[0]"                 [#{"store"} #{"book"} #{0}]
      "$.store.book[0, 1]"              [#{"store"} #{"book"} #{0 1}]
      "$.store.book[*]"                 [#{"store"} #{"book"} '*]
      "$.store['book'].*"               [#{"store"} #{"book"} '*]
      "$['store']['book'][0]"           [#{"store"} #{"book"} #{0}]
      "$['store']['book'][0][0]"        [#{"store"} #{"book"} #{0} #{0}]
      "$['store']['book'][0 , 1]"       [#{"store"} #{"book"} #{0, 1}]
      "$['store']['book'][*]"           [#{"store"} #{"book"} '*]
      "$['store']['book'][0]['author']" [#{"store"} #{"book"} #{0} #{"author"}]
      "$['store']['book']['*'][0]"      [#{"store"} #{"book"} #{"*"} #{0}]
      "$.book[-1]"                      [#{"book"} #{-1}]
      ;; Array slicing
      "$.book[0:6:2]"   [#{"book"} #{{:start 0 :end 6 :step 2 :bounded? true}}]
      "$.book[-1:]"     [#{"book"} #{{:start -1 :end max-int :step 1 :bounded? false}}]
      "$.book[:1]"      [#{"book"} #{{:start 0 :end 1 :step 1 :bounded? true}}]
      "$.book[0:2]"     [#{"book"} #{{:start 0 :end 2 :step 1 :bounded? true}}]
      "$.book[0:3:]"    [#{"book"} #{{:start 0 :end 3 :step 1 :bounded? true}}]
      "$.book[0:4:2,1]" [#{"book"} #{{:start 0 :end 4 :step 2 :bounded? true} 1}])
    (are [path] (= nil (parse-first path))
      "$.store.book[[0]"
      "$.store.book[0]]"
      "$..book[?(@isbn)]" ; Filter fns not supported in xAPI Profile spec
      "$..book[l33t]")))

(deftest parse-test-4
  (testing "JSONPath strings that have been joined with '|'"
    (are [paths v] (= v (parse paths))
      "$.store.book|$.results.extensions"
      [[#{"store"} #{"book"}] [#{"results"} #{"extensions"}]]
      "$.store.book  |$.results.extensions"
      [[#{"store"} #{"book"}] [#{"results"} #{"extensions"}]]
      "$.store.book|  $.results.extensions"
      [[#{"store"} #{"book"}] [#{"results"} #{"extensions"}]]
      "$.store.book | $.results.extensions"
      [[#{"store"} #{"book"}] [#{"results"} #{"extensions"}]]
      "$['store']['book']|$['results']['extensions']"
      [[#{"store"} #{"book"}] [#{"results"} #{"extensions"}]]
      "$['store']['book'] | $['results']['extensions']"
      [[#{"store"} #{"book"}] [#{"results"} #{"extensions"}]]
      "$['store']['book'][0] | $.results.extensions['https://foo.org']"
      [[#{"store"} #{"book"} #{0}] [#{"results"} #{"extensions"} #{"https://foo.org"}]]
      "$['store   |$.io']"
      [[#{"store   |$.io"}]]
      "$['store|$.io']"
      [[#{"store|$.io"}]]
      "$.store[*] | $.results['b|ah']"
      [[#{"store"} '*] [#{"results"} #{"b|ah"}]])
    (are [paths] (= nil (parse paths))
      "$.store.book $.results.extension"
      "$.store.book | what the pineapple"
      "$['store|$['io']]")))
