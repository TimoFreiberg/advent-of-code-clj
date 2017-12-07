(ns advent-of-code.day7 (:require [advent-of-code.main :refer :all]))

(defn parse-line [line]
  (let [[base children] (clojure.string/split line #"->")
        [name weight] (split-words base)]
    (run! println [base children name weight])))

(defn solve-1 [input]
  (->> input
       (clojure.string/split-lines)
       (mapv parse-line)

       (run! println)))

(defn solve-2 [input]
  )

(defn solve-day-7 []
  (let [input (load-input-file "7")]
    (println (solve-1 input))
    (println (solve-2 input))))
