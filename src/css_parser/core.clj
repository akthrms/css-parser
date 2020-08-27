(ns css-parser.core)

(defn any [input]
  (if-let [c (first input)]
    (list [c (.substring input 1)])
    '()))

(defn failure [_] '())

; (any "clojure-1.9")
; => ([\c "lojure-1.9"])

(defn parse [parser input]
  (parser input))

(defn parse-all [parser input]
  (->> input
       (parse parser)
       (filter #(= "" (second %)))
       (ffirst)))
