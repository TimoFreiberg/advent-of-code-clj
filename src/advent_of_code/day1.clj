(ns advent-of-code.day1 (:require [advent-of-code.main :refer [load-input-file]]))

(defn string->ints [s]
  (->> s
       (filter #(Character/isDigit %))
       (map (comp read-string str))))

(defn shift-right
  ([coll] (shift-right 1 coll))
  ([shift-by coll] (drop shift-by (cycle coll))))

(defn pair-with-next [coll]
  (mapv vector coll (shift-right coll)))

(defn pair-with-halfway [coll]
  (mapv vector coll (shift-right (/ (count coll) 2) coll)))

(defn both-equal [coll]
  (= (first coll) (second coll)))

(defn sum-matching-pairs [pairs]
  (->> pairs
       (filter both-equal)
       (map first)
       (reduce +)))

(defn solve-1 [input]
  (->> input
       (string->ints)
       (pair-with-next)
       (sum-matching-pairs)))

(defn solve-2 [input]
  (->> input
       (string->ints)
       (pair-with-halfway)
       (sum-matching-pairs)))

(defn solve-day-1 []
  (let [input (load-input-file "1")]
    (println (solve-1 input))
    (println (solve-2 input))))
