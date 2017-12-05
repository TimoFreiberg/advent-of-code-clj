(ns advent-of-code.day5 (:require [advent-of-code.main :refer [load-input-file find-first]]))

(defn solve-1 [input]
  (->> input
       (clojure.string/split-lines)
       (mapv #(Integer/parseInt %))
       (reduce +)))

(defn solve-2 [input]
  )


(defn solve-day-5 []
  (let [input (load-input-file "5")]
    (println (solve-1 input))
    (println (solve-2 input))))
