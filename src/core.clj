(ns core
  (:require [css-parser.core :as parser]))

(defn parse-css [input]
  (parser/parse-all parser/ruleset input))

(defn -main []
  (println (parse-css (slurp "sample.css"))))
