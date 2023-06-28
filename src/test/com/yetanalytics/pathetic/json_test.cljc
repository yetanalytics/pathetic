(ns com.yetanalytics.pathetic.json-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.test.check]
            [clojure.test.check.properties]
            [clojure.spec.test.alpha :as stest]
            [com.yetanalytics.pathetic.json :as json
             :refer [recursive-descent jassoc jassoc-in]]))

(def store-ex {"store"
               {"book"
                [{"category" "reference"
                  "author"   "Nigel Rees"
                  "title"    "Sayings of the Century"}
                 {"category" "fiction"
                  "author"   "Evelyn Waugh"
                  "title"    "Sword of Honour"}
                 {"category" "fiction"
                  "author"   "Herman Melville"
                  "title"    "Moby Dick"}
                 {"category" "fiction"
                  "author"   "J.R.R. Tolkien"
                  "title"    "The Lord of the Rings"}]
                "bicycle"
                {"color" "red"
                 "price" 20}}})

(deftest spec-test
  (testing "json-related specs"
    (is (s/valid? ::json/json store-ex))
    (is (s/valid? ::json/json nil))
    (is (s/valid? ::json/path [1 2 3 "foo" "bar"]))))

(deftest recursive-descent-test
  (testing "recursive-descent function - should return all JSON substructures"
    (is (= [{:json {"store"
                    {"book"
                     [{"category" "reference" "author" "Nigel Rees" "title" "Sayings of the Century"}
                      {"category" "fiction" "author" "Evelyn Waugh" "title" "Sword of Honour"}
                      {"category" "fiction" "author" "Herman Melville" "title" "Moby Dick"}
                      {"category" "fiction" "author" "J.R.R. Tolkien" "title" "The Lord of the Rings"}]
                     "bicycle" {"color" "red" "price" 20}}}
             :path []}
            {:json {"book"
                    [{"category" "reference" "author" "Nigel Rees" "title" "Sayings of the Century"}
                     {"category" "fiction" "author" "Evelyn Waugh" "title" "Sword of Honour"}
                     {"category" "fiction" "author" "Herman Melville" "title" "Moby Dick"}
                     {"category" "fiction" "author" "J.R.R. Tolkien" "title" "The Lord of the Rings"}]
                    "bicycle" {"color" "red" "price" 20}}
             :path ["store"]}
            {:json [{"category" "reference" "author" "Nigel Rees" "title" "Sayings of the Century"}
                    {"category" "fiction" "author" "Evelyn Waugh" "title" "Sword of Honour"}
                    {"category" "fiction" "author" "Herman Melville" "title" "Moby Dick"}
                    {"category" "fiction" "author" "J.R.R. Tolkien" "title" "The Lord of the Rings"}]
             :path ["store" "book"]}
            {:json {"category" "reference" "author" "Nigel Rees" "title" "Sayings of the Century"}
             :path ["store" "book" 0]}
            {:json "reference"
             :path ["store" "book" 0 "category"]}
            {:json "Nigel Rees"
             :path ["store" "book" 0 "author"]}
            {:json "Sayings of the Century"
             :path ["store" "book" 0 "title"]}
            {:json {"category" "fiction" "author" "Evelyn Waugh" "title" "Sword of Honour"}
             :path ["store" "book" 1]}
            {:json "fiction"
             :path ["store" "book" 1 "category"]}
            {:json "Evelyn Waugh"
             :path ["store" "book" 1 "author"]}
            {:json "Sword of Honour"
             :path ["store" "book" 1 "title"]}
            {:json {"category" "fiction" "author" "Herman Melville" "title" "Moby Dick"}
             :path ["store" "book" 2]}
            {:json "fiction"
             :path ["store" "book" 2 "category"]}
            {:json "Herman Melville"
             :path ["store" "book" 2 "author"]}
            {:json "Moby Dick"
             :path ["store" "book" 2 "title"]}
            {:json {"category" "fiction" "author" "J.R.R. Tolkien" "title" "The Lord of the Rings"}
             :path ["store" "book" 3]}
            {:json "fiction"
             :path ["store" "book" 3 "category"]}
            {:json "J.R.R. Tolkien"
             :path ["store" "book" 3 "author"]}
            {:json "The Lord of the Rings"
             :path ["store" "book" 3 "title"]}
            {:json {"color" "red" "price" 20}
             :path ["store" "bicycle"]}
            {:json "red"
             :path ["store" "bicycle" "color"]}
            {:json 20
             :path ["store" "bicycle" "price"]}]
           (recursive-descent store-ex)))))

(deftest jassoc-test
  (testing "jassoc function"
    (is (= {"0" "a"}
           (jassoc nil "0" "a")))
    (is (= ["a"]
           (jassoc nil 0 "a")))
    (is (= [nil "a"]
           (jassoc nil 1 "a")))
    (is (= ["a" nil :c]
           (jassoc ["a"] 2 :c)))
    (is (= {"foo" "b"}
           (jassoc ["a"] "foo" "b")))))

(deftest jassoc-in-test
  (testing "jassoc-in function"
    (is (= "b"
           (jassoc-in nil [] "b")))
    (is (= "b"
           (jassoc-in {} [] "b")))
    (is (= "b"
           (jassoc-in {"foo" "bar"} [] "b")))
    (is (= {"0" "a"}
           (jassoc-in nil ["0"] "a")))
    (is (= {"foo" {"bar" "b"}}
           (jassoc-in {"foo" {"bar" "a"}} ["foo" "bar"] "b")))
    (is (= {"foo" {"bar" "a" "baz" "b"}}
           (jassoc-in {"foo" {"bar" "a"}} ["foo" "baz"] "b")))
    (is (= {"foo" ["b"]}
           (jassoc-in {"foo" {"bar" "a"}} ["foo" 0] "b")))
    (is (= {"foo" "b"}
           (jassoc-in nil ["foo"] "b")))
    (is (= [nil nil [nil nil nil 3]]
           (jassoc-in nil [2 3] 3)))))

(deftest gen-tests
  (testing "Generative tests for json namespace"
    (let [results
          (stest/check `[json/recursive-descent
                         json/jassoc
                         json/jassoc-in]
                       {:clojure.spec.test.check/opts
                        {:num-tests #?(:clj 500 :cljs 100)
                         :seed (rand-int 2000000000)}})
          {:keys [total check-passed]}
          (stest/summarize-results results)]
      (is (= total check-passed)))))
