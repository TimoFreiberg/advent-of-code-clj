(ns advent-of-code.day6 (:require [advent-of-code.main :refer :all]))

(defn redistribute [int-vec]
  (let [max-val (apply max int-vec)
        max-index (.indexOf int-vec max-val)
        emptied-vec (assoc int-vec max-index 0)
        inc-index (fn [index] (mod (inc index) (count int-vec)))
        perform-redistribution (fn [index vals-left vec]
                                 (if (= 0 vals-left)
                                   vec
                                   (recur (inc-index index)
                                          (dec vals-left)
                                          (update vec index inc))))]
    (perform-redistribution (inc-index max-index) max-val emptied-vec)))

(defn count-uniques-step [[vals n] val]
  (if (contains? vals val) (reduced n) [(conj vals val) (inc n)]) )
(defn count-uniques [vals]
  (reduce count-uniques-step [#{} 0] vals))

(defn get-unique-step [[vals n] val]
  (if (contains? vals val) (reduced val) [(conj vals val) (inc n)]) )
(defn get-first-unique [vals]
  (reduce get-unique-step [#{} 0] vals))

(defn solve-1 [input]
  (->> input (split-words)
       (mapv #(Integer/parseInt %))
       (iterate redistribute)
       (count-uniques)))

(defn solve-2 [input]
  (let [redistributions (->> input
                             (split-words)
                             (mapv #(Integer/parseInt %))
                             (iterate redistribute))
        first-dup (get-first-unique redistributions)
        cycle-start (drop-while #(not= first-dup %) redistributions)]
    (count-uniques cycle-start)))


(defn solve-day-6 []
  (let [input (load-input-file "6")]
    (println (solve-1 input))
    (println (solve-2 input))))
