(ns css_parser.core_test
  (:require [clojure.test :refer :all]
            [css-parser.core :as parser]))

(deftest any
  (is (= '([\c "lojure"]) (parser/any "clojure")))
  (is (= '() (parser/any ""))))

(deftest failure
  (is (= '() (parser/failure "test"))))

(deftest parse
  (is (= '([\c "lojure"]) (parser/parse parser/any "clojure"))))

(deftest parse-all
  (is (= \t (parser/parse-all parser/any "t")))
  (is (= nil (parser/parse-all parser/any "test"))))

(deftest return
  (is (= '([\c "lojure"]) ((parser/return \c) "lojure"))))

; run test command
; (run-tests)
