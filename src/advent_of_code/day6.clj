(ns advent-of-code.day6 (:require [advent-of-code.main :refer :all]))

(defn redistribute [])
(defn solve-1 [input]
  (->> input (split #"\s+")
       (mapv #(Integer/parseInt %))
       (iterate redistribute)))
(def test-vec [0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11])

(defn solve-2 [input]
  )


(defn solve-day-6 []
  (let [input (load-input-file "6")]
    (println (solve-1 input))
    (println (solve-2 input))))
