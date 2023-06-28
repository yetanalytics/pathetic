(ns com.yetanalytics.pathetic.json-path-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check]
            [clojure.test.check.properties]
            [clojure.spec.test.alpha :as stest]
            [com.yetanalytics.pathetic.path :as path]))

;; Note: path-seqs and speculative-path-seqs are tested indirectly in
;; pathetic-test.

;; TODO: Add direct unit tests, however

(deftest gen-tests
  (testing "Generative tests for json-path"
    (let [results
          (stest/check `[path/path-seqs
                         path/speculative-path-seqs]
                       {:clojure.spec.test.check/opts
                        {:num-tests #?(:clj 100 :cljs 20)
                         :seed (rand-int 2000000000)}})
          {:keys [total check-passed]}
          (stest/summarize-results results)]
      (is (= total check-passed)))))
