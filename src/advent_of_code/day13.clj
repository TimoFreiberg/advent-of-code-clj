(ns advent-of-code.day13 (:require [advent-of-code.main :refer :all]))


(defn parse-line [s]
  (let [[pos range] (mapv #(Integer/parseInt (.trim %)) (clojure.string/split s #":"))]
    {:pos pos :val 0 :range range}))


(defn layer-val-at-pos [layer]
  (let [{pos :pos val :val range :range} layer
        abs-val (mod (+ (:pos layer) (:val layer)) (:range layer))]
    (if (even? (quot pos range))
      abs-val
      (- range abs-val))))

(defn caught-severity [layer]
  (if (= 0 (layer-val-at-pos layer))
    (* (:pos layer) (:range layer))
    0))

(defn solve-1 [input]
  (->> input
       (clojure.string/split-lines)
       (mapv (comp trace caught-severity trace parse-line trace))
       ;; (mapv caught-severity)
       (trace)
       (reduce + 0)))

(defn solve-2 [input]
  )

(def input (load-input-file "13"))


(defn solve-day-13 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
