(ns advent-of-code.day7 (:require [advent-of-code.main :refer :all]))

(defn split-commas [s] (clojure.string/split s #","))

(defn parse-line [line]
  (let [[base children] (clojure.string/split line #"->")
        [name weight] (split-words base)
        children-vec (->> children
                          (fnil split-commas "")
                          (map clojure.string/trim))]
    [name children-vec]))

(defn solve-1 [input]
  (->> input
       (clojure.string/split-lines)
       (take 10)
       (trace)
       (mapv parse-line)))

(defn solve-2 [input])

(defn solve-day-7 []
  (let [input (load-input-file "7")]
    (println (solve-1 input))
    (println (solve-2 input))))
