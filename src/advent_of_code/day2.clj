(ns advent-of-code.day2 (:require [advent-of-code.main :refer [load-input-file]]))


(defn solve-day-2
  (let [input (load-input-file "2")]
    (println (solve-1 input))
    (println (solve-2 input))))

