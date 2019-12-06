(ns com.yetanalytics.pathetic.json-path-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.pathetic.json-path :refer :all]
            [blancas.kern.core :as k]))

(deftest parse-test
  (are [path v] (= v
                   (parse path))
    "$.store.book[*].author" [#{"store"} #{"book"} '* #{"author"}]
    "$..author"              ['* #{"author"}]
    "$.store.*"              [#{"store"} '*]
    "$.store..price"         [#{"store"} '* #{"price"}]
    "$..book[2]"             ['* #{"book"} #{2}]
    "$..book[-1:]"           ['* #{"book"} (->RangeSpec -1 9223372036854775807 1)]
    "$..book[0,1]"           ['* #{"book"} #{0 1}]
    "$..book[:2]"            ['* #{"book"} (->RangeSpec 0 2 1)]
    "$..*"                   '[* *]
    ))
