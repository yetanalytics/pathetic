(ns com.yetanalytics.pathetic-test-macros
  (:require [clojure.java.io   :as io]
            [clojure.data.json :as json]))

(defmacro read-long-statement []
  (with-open
   [r (io/reader (io/resource "pathetic/data/long.json"))]
    (json/read r)))

(defmacro parse-failed? [form]
  (if (:ns &env) ;; ClojureScript
    `(try ~form
          (catch js/Error e# (= "Cannot parse JSONPath string"
                                (ex-message e#))))
    `(try ~form
          (catch Exception e# (= "Cannot parse JSONPath string"
                                 (ex-message e#))))))

(defmacro strict-parse-failed? [form]
  (if (:ns &env) ;; ClojureScript
    `(try ~form
          (catch js/Error e# (= "Illegal path element in strict mode"
                                (ex-message e#))))
    `(try ~form
          (catch Exception e# (= "Illegal path element in strict mode"
                                 (ex-message e#))))))
