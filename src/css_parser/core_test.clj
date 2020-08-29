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

(deftest match
  (is (= '([\c "lojure"]) (parser/parse (parser/match "c") "clojure"))))

(deftest letter
  (is (= '([\c "lojure"]) (parser/parse parser/letter "clojure"))))

(deftest digit
  (is (= '([\1 "00"]) (parser/parse parser/digit "100"))))

(deftest space
  (is (= '([\space "clojure"]) (parser/parse parser/space " clojure"))))

(deftest combinators
  (let [clojure-version (parser/do*
                          (parser/string "clojure")
                          (parser/match " ")
                          (major <- parser/digit)
                          (parser/match ".")
                          (minor <- parser/digit)
                          (parser/return (str "major: " major "; minor: " minor)))]
    (is (= "major: 1; minor: 7" (parser/parse-all clojure-version "clojure 1.7")))))

(deftest letter-or-hyphen
  (is (= '([\c "lojure"]) (parser/parse parser/letter-or-hyphen "clojure")))
  (is (= '([\- "clojure"]) (parser/parse parser/letter-or-hyphen "-clojure"))))

(deftest css
  (is (= #css_parser.core.Rule{:key "background", :value "#fafafa"}
         (parser/parse-all parser/rule "background: #fafafa;")))
  (is (= #css_parser.core.Ruleset{:selector "p",
                                  :rules    (#css_parser.core.Rule{:key "background", :value "#fafafa"} #css_parser.core.Rule{:key "color", :value "red"})}
         (parser/parse-all parser/ruleset "p { background: #fafafa; color: red; }"))))

; run test command
; (run-tests)
