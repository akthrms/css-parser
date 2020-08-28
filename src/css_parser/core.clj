(ns css-parser.core
  (:import [clojure.lang Symbol]))

;; ------------------------------
;; 1. Parser abstraction
;; ------------------------------

(defn any [input]
  (if-let [c (first input)]
    (list [c (.substring input 1)])
    '()))

(defn failure [_]
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

(defn return [v]
  (fn [input] (list [v input])))

(defn >>= [m f]
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

(defn char-cmp [f]
  (fn [c] (sat (partial f (first c)))))

(def match (char-cmp =))

(def none-of (char-cmp not=))

(defn from-re [re]
  (sat (fn [v] (not (nil? (re-find re (str v)))))))

(def digit (from-re #"[0-9]"))

(def letter (from-re #"[a-zA-Z]"))

;; ------------------------------
;; 4. Combinators
;; ------------------------------

(defn and-then [p1 p2]
  (do*
    (r1 <- p1)
    (r2 <- p2)
    (return (str r1 r2))))

(defn or-else [p1 p2]
  (fn [input]
    (lazy-cat (parse p1 input) (parse p2 input))))

(declare plus)

(declare optional)

(defn many [parser]
  (optional (plus parser)))

(defn plus [parser]
  (do*
    (a <- parser)
    (as <- (many parser))
    (return (cons a as))))

(defn optional [parser]
  (or-else parser (return "")))

(def space (or-else (match " ") (match "\n")))

(def spaces (many space))

(defn string [s]
  (reduce and-then (map #(match (str %)) s)))

;; ------------------------------
;; 5. CSS
;; ------------------------------

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
