(ns core
  (:require [css-parser.core :refer [parse-all ruleset]]))

(defn -main []
  (parse-all ruleset (slurp "sample.css")))
