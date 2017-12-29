(ns advent-of-code.day19 (:require
                          [advent-of-code.main :refer :all]))

(defn letter? ^Character [x]
  (and
   (char? x)
   (Character/isLetter x)))

(defn line? [char]
  (or
   (some #(= char %) [\- \+ \|])
   (letter? char)))

(defn find-starting-pos [grid]
  (let [top-line (first grid)]
    (loop [pos 0]
      (if (line? (get top-line pos))
        pos
        (recur (inc pos))))))

(defn move-pos [pos dir]
  (let [[x y] pos]
    [(case dir
       :up (dec x)
       :down (inc x)
       x)
     (case dir
       :right (inc y)
       :left (dec y)
       y)]))

(defn neighbors [pos dir]
  (case dir
    (:up :down) (map #(vector % (move-pos pos %)) [:left :right])
    (:left :right) (map #(vector % (move-pos pos %)) [:up :down])
    (throw (IllegalArgumentException. (str "Illegal dir " dir)))))

(defn find-turn-dir [grid {:keys [pos dir]}]
  (let [valid-neighbors (filter
                         #(line? (get-in grid (second %)))
                         (neighbors pos dir))]
    (case (count valid-neighbors)
      0 (do
          (trace-pprint "stopping at" pos)
          [dir (move-pos pos dir)])
      1 (first valid-neighbors)
      2 (do
          (trace-pprint
           "WARN: found fork ("
           valid-neighbors
           "), choosing"
           (first valid-neighbors))
          (first valid-neighbors)))))

(defn walk-step [grid state]
  (trace-pprint :walk-step state)
  (let [{:keys [pos dir letters]} state
        current-field (get-in grid pos)
        state (update state :steps inc)
        state (if (not (line? current-field))
                (assoc state :stopped true)
                state)
        state (if (letter? current-field)
                (update state :letters conj current-field)
                state)
        next-pos (move-pos pos dir)
        [turn-dir next-pos] (if (line? (get-in grid next-pos))
                   [dir next-pos]
                   (find-turn-dir grid state))
        _ (when (not= turn-dir dir)
            (trace-pprint "turning" turn-dir))
        state (assoc state
                     :dir turn-dir
                     :pos next-pos)]
    state))

(defn walk-the-line [grid]
  (trace-pprint :walk-the-line)
  (take-while
   #(not (:stopped %))
   (iterate
    (partial walk-step grid)
    {:pos [0 (find-starting-pos grid)]
     :dir :down
     :letters []
     :steps 0})))

(defn solve-1 [input]
  (->> input
       (clojure.string/split-lines)
       (walk-the-line)
       (last)
       (:letters)
       (apply str)))

(defn solve-2 [input]
  (->> input
       (clojure.string/split-lines)
       (walk-the-line)
       (last)
       (:steps)))

(def input (load-input-file "19"))

(defn solve-day-19 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
