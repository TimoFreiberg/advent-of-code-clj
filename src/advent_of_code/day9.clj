(ns advent-of-code.day9 (:require [advent-of-code.main :refer :all]))



; {} group
; (<)+> garbage, ignored
; ! next char is ignored (even a next !)
; points: 1 for top level group, n+1 for nested groups

(defn step [state c]
  (if (get state :ignoring)
    (assoc state :ignoring false)
    (case c
      \! (assoc state :ignoring true)
      (case (get state :mode)
        :garbage (case c
                   \> (assoc state :mode :normal)
                   (update state :garbage-count inc))
        (case c
          \{ (update state :current-point-level inc)
          \} (-> state
                 (update :score #(+ % (get state :current-point-level)))
                 (update :current-point-level dec))
          \< (assoc state :mode :garbage)
          state)))))

(def init-state
  {:score 0 :current-point-level 0 :mode :normal :ignoring false :garbage-count 0})

(defn debug-1 [input]
  (clojure.pprint/pprint
   (take 50 (mapv vector input (drop 1 (reductions step init-state input))))))

(defn solve-1 [input]
  (reduce step init-state input)
  )

(defn solve-2 [input]
  )

(defn solve-day-9 []
  (let [input (load-input-file "9")]
    (clojure.pprint/pprint (solve-1 input))
    (clojure.pprint/pprint (solve-2 input))))
