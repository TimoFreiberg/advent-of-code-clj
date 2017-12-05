(ns advent-of-code.day4 (:require [advent-of-code.main :refer [load-input-file find-first]]))


(defn words [s]
  (clojure.string/split s #"\s+"))

(defn valid [phrase]
  (not= :invalid
     (reduce (fn [existing-words word] (if (contains? existing-words word) (reduced :invalid) (conj existing-words word))) #{} phrase)))

(defn sort-str [s]
  (apply str (sort s)))

(defn solve-1 [input]
  (->> input
       (clojure.string/split-lines)
       (map words)
       (filter valid)
       (count)))

(defn solve-2 [input]
  (->> input
       (clojure.string/split-lines)
       (map words)
       (map #(map sort-str %))
       (filter valid)
       (count)))

(defn solve-day-4 []
  (let [input (load-input-file "4")]
    (println (solve-1 input))
    (println (solve-2 input))))
