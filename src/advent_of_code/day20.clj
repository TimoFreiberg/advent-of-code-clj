(ns advent-of-code.day20)


(defn solve-1 [input]
  (->> input
       (clojure.string/split-lines)
       (walk-the-line)
       (last)
       (:letters)
       (apply str)))

(defn solve-2 [input]
  (->> input
       (clojure.string/split-lines)
       (walk-the-line)
       (last)
       (:steps)))

(def input (load-input-file "19"))

(defn solve-day-19 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
