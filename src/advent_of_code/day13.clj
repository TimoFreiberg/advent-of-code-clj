(ns advent-of-code.day13 (:require [advent-of-code.main :refer :all]))


(defn parse-line [s]
  (let [[pos range] (mapv #(Integer/parseInt (.trim %)) (clojure.string/split s #":"))]
    {:pos pos :val 0 :range range}))


(defn cycle-range [n]
  (concat (range n) (range (- n 2) 0 -1)))

(defn layer-val-at-pos [layer]
  (let [-cycle (cycle-range (:range layer))
        pos (:pos layer)]
    (nth -cycle (mod pos (count -cycle)))))


(defn caught-severity [layer]
  (if (= 0 (layer-val-at-pos layer))
    (* (:pos layer) (:range layer))
    0))

(defn parse-input [input]
  (->> input
       (clojure.string/split-lines)
       (mapv parse-line)))

(defn inc-pos [layer]
  (update layer :pos inc))

(defn sum-severity [layers]
  (->> layers
       (mapv caught-severity)
       (reduce + 0)))

(defn solve-1 [input]
  (->> input
       (parse-input)
       (sum-severity)))

(defn solve-2 [input]
  (let [layers (parse-input input)]
    (count
     (take-while
      (fn [ls]
        (let [sev (sum-severity ls)]
          (println ls)
          (println sev)
          (not= 0 sev)))
      (iterate #(mapv inc-pos %) layers)))))

(def input (load-input-file "13"))

(defn poses-and-cycle-counts [-input]
  (mapv
   (fn [l]
     [:pos (:pos l)
      :count (count (cycle-range (:range l)))])
   (parse-input -input)))

(defn solve-day-13 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
