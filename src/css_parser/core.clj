(ns css-parser.core
  (:import [clojure.lang Symbol]))

;; ------------------------------
;; 1. Parser abstraction
;; ------------------------------

; type Parser a = String -> [(a, String)]

(defn any
  "takes any input and 'consume' first char from it"
  [input]
  (if-let [c (first input)]
    (list [c (.substring input 1)])
    '()))

(defn failure
  "this one doesn't accept any input"
  [_]
  '())

(defn parse [parser input]
  (parser input))

(defn parse-all [parser input]
  (->> input
       (parse parser)
       (filter #(= "" (second %)))
       (ffirst)))

;; ------------------------------
;; 2. Monads
;; ------------------------------

(defn return
  "builds parser that always returns given element without consuming (changing) input"
  [v]
  (fn [input] (list [v input])))

(defn >>=
  "takes parser and function that builds new parsers from (each) result of applying first one"
  [m f]
  (fn [input]
    (->> input
         (parse m)
         (mapcat (fn [[v tail]] (parse (f v) tail))))))

(defn merge-bind [body bind]
  (if (and (not= Symbol (type bind))
           (= 3 (count bind))
           (= '<- (second bind)))
    `(>>= ~(last bind) (fn [~(first bind)] ~body))
    `(>>= ~bind (fn [~'_] ~body))))

(defmacro do* [& forms]
  (reduce merge-bind (last forms) (reverse (butlast forms))))

;; ------------------------------
;; 3. Basic parsers
;; ------------------------------

(defn sat [pred]
  (>>= any (fn [v] (if (pred v) (return v) failure))))

(defn char-cmp
  "just a helper"
  [f]
  (fn [c] (sat (partial f (first c)))))

(def match
  "recognizes given char"
  (char-cmp =))

(def none-of
  "rejects given char"
  (char-cmp not=))

(defn from-re
  "just a helper"
  [re]
  (sat (fn [v] (not (nil? (re-find re (str v)))))))

(def digit
  "recognizes any digit"
  (from-re #"[0-9]"))

(def letter
  "recognizes any letter"
  (from-re #"[a-zA-Z]"))

;; ------------------------------
;; 4. Combinators
;; ------------------------------

(defn and-then
  "(ab)"
  [p1 p2]
  (do*
    (r1 <- p1)
    (r2 <- p2)
    (return (str r1 r2))))

(defn or-else
  "(a|b)"
  [p1 p2]
  (fn [input]
    (lazy-cat (parse p1 input) (parse p2 input))))

(declare plus)

(declare optional)

(defn many
  "(a*)"
  [parser]
  (optional (plus parser)))

(defn plus
  "(a+) equals to (aa*)"
  [parser]
  (do*
    (a <- parser)
    (as <- (many parser))
    (return (cons a as))))

(defn optional
  "(a?)"
  [parser]
  (or-else parser (return "")))

(def space
  "recognizes space (or newline)"
  (or-else (match " ") (match "\n")))

(def spaces
  "recognizes empty string or arbitrary number of spaces"
  (many space))

(defn string
  "recognizes given string, i.e. 'clojure'"
  [s]
  (reduce and-then (map #(match (str %)) s)))

;; ------------------------------
;; 5. CSS
;; ------------------------------

; type Selector = String
; data Rule = Rule String String deriving Show
; data Ruleset = Ruleset Selector [Rule] deriving Show

(defrecord Rule [key value])

(defrecord Ruleset [selector rules])

(def letter+ (or-else letter (match "-")))

(def rule
  (do*
    (p <- (many (none-of ":")))
    (match ":")
    spaces
    (v <- (many (none-of ";")))
    (match ";")
    spaces
    (return (->Rule (apply str p) (apply str v)))))

(def ruleset
  (do*
    (s <- (plus (none-of "{")))
    (match "{")
    spaces
    (r <- (plus rule))
    spaces
    (match "}")
    spaces
    (return (->Ruleset (.trim (apply str s)) r))))
