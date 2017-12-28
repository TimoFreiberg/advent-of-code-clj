(ns advent-of-code.day19 (:require
                          [advent-of-code.main :refer :all]))

(defn line? [char]
  (not= char \space))

(defn find-starting-position [grid]
  (let [top-line (first grid)]
    (loop [position 0]
      (if (line? (get top-line position))
        position
        (recur (inc position))))))

(defn walk-step [grid state]
  (let [{:keys [position dir letters]} state
        current-field (get-in grid position)
        state (if (Character/isAlphabetic current-field)
                (update state :letters conj current-field)
                state)]))

(defn walk-the-line [grid]
  (take-while
   #(not (:stopped %))
   (map
    #(if *verbose* % (dissoc % :visited-positions))
    (iterate
     (partial walk-step grid)
     {:position [0 (find-starting-position grid)]
      :dir :down
      :letters []}))))

(defn solve-1 [input]
  (->> input
       (clojure.string/split-lines)
       (walk-the-line)))

(defn solve-2 [input]
  )

(def input (load-input-file "19"))

(defn solve-day-19 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
