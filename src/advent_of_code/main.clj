(ns advent-of-code.main)

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn trace [s]
  (do
    (clojure.pprint/pprint s)
    s))

(defn split-words [s] (clojure.string/split s #"\s+"))

(defn string-to-int [s]
  (->> s
       (filter #(Character/isDigit %))
       (apply str)
       (Integer/parseInt)))

(defn load-input-file "load-input-file [day-number] loads content of the input file for the given day " [day-number]
  (slurp (str "resources/day" day-number)))
