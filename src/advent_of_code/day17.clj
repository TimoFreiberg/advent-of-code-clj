(ns advent-of-code.day17 (:require [advent-of-code.main :refer :all]))

(defn insert [vec pos item]
  (apply conj (subvec vec 0 pos) item (subvec vec pos)))

(defn run-buffer [steps until-val]
  (loop [buffer [0]
         position 0
         next-val 1]
    (if (> next-val until-val)
      {:buffer buffer :position position :next-val next-val}
      (let [next-pos (trace :next-pos
                            (inc (mod (+ position steps) (count buffer)))
                            )
            next-buffer (trace :next-buffer
                               (insert buffer next-pos next-val))]
        (recur next-buffer next-pos (inc next-val))))))

(defn simulate-buffer-focusing-on-second-element [steps until-val]
  (loop [position 0
         next-val 1
         last-second-element nil]
    (if (> next-val until-val)
      {:position position :next-val next-val :last-second-element last-second-element}
      (let [next-pos (inc (mod (+ position steps) next-val))
            new-second-element (if (= 1 next-pos)
                                 next-val
                                 last-second-element)]
        (recur next-pos (inc next-val) new-second-element)))))

(defn solve-1 [steps]
  (let [{buffer :buffer position :position} (run-buffer steps 2017)]
    (get buffer (mod (inc position) (count buffer)))))

(defn solve-2 [steps]
  (simulate-buffer-focusing-on-second-element steps 50000000))


(def input 349)

(defn solve-day-17 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
