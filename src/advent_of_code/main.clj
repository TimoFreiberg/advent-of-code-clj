(ns advent-of-code.main)

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn trace [& things]
  (doseq [s things]
    (clojure.pprint/pprint s))
  (last things))

(defn split-words [s] (clojure.string/split s #"\s+"))

(defn string-to-int [s]
  (->> s
       (filter #(Character/isDigit %))
       (apply str)
       (Integer/parseInt)))

(defn flip [f & args]
  (apply f (reverse args)))

(defn load-input-file "load-input-file [day-number] loads content of the input file for the given day " [day-number]
  (slurp (str "resources/day" day-number)))
