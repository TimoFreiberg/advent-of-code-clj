(ns advent-of-code.day14 (:require [advent-of-code.main :refer :all]
                                    [advent-of-code.knot-hash :refer [knot-hash]]))

(def input "ffayrhll")

(defn bin-knot-hash [k]
  (knot-hash k 2))

(defn hashes [n k]
  (mapcat #(bin-knot-hash (str k "-" %)) (range n)))

(defn not-zero? [c]
  (not= \0 c))

(defn count-ones [s]
  (count (filter not-zero? s)))

(defn solve-1 [input]
  (->> input
      (hashes 128)
      (trace)
      (map count-ones)
      (reduce + 0)))

(defn solve-2 [input]
  )



(defn solve-day-14 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
