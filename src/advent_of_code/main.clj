(ns advent-of-code.main)

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn split [regex string] (clojure.string/split string regex))

(defn load-input-file "load-input-file [day-number] loads content of the input file for the given day " [day-number]
  (slurp (str "resources/day" day-number)))
