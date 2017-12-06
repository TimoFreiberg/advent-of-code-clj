(ns advent-of-code.day5 (:require [advent-of-code.main :refer [load-input-file find-first]]))

(defn in-bounds [index coll]
  (not (or (< index 0) (>= index (count coll)))))

(defn count-jumps [mod-field-fun curr-count index jump-vec]
  (let [jumps (get jump-vec index)
        new-vec (update jump-vec index mod-field-fun)
        new-index (+ index jumps)
        new-count (inc curr-count)]
    (if (in-bounds new-index new-vec)
      (recur mod-field-fun new-count new-index new-vec)
      new-count)))

(defn solve-1 [input]
  (->> input
       (clojure.string/split-lines)
       (mapv #(Integer/parseInt %))
       (count-jumps inc 0 0)))

(defn inc-or-dec-around-3 [x]
  (if (< x 3)
    (inc x)
    (dec x)))

(defn solve-2 [input]
  (->> input
       (clojure.string/split-lines)
       (mapv #(Integer/parseInt %))
       (count-jumps inc-or-dec-around-3 0 0)))


(defn solve-day-5 []
  (let [input (load-input-file "5")]
    (println (solve-1 input))
    (println (solve-2 input))))
