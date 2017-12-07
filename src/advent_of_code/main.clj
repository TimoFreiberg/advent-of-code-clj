(ns advent-of-code.main)

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn trace [s]
  (do
    (println s)
    s))

(defn split-words [s] (clojure.string/split s #"\s+"))

(defn load-input-file "load-input-file [day-number] loads content of the input file for the given day " [day-number]
  (slurp (str "resources/day" day-number)))
