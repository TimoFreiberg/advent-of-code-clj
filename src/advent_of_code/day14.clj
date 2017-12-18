(ns advent-of-code.day14 (:require [advent-of-code.main :refer :all]
                                   [advent-of-code.knot-hash :refer [knot-hash-bin]]))

(def input "ffayrhll")

(defn hashes [n k]
  (mapv #(knot-hash-bin (str k "-" %)) (range n)))

(defn not-zero? [c]
  (not= \0 c))

(defn count-ones [s]
  (count (filter not-zero? s)))

(defn find-coord [val grid-map]
  (first
   (find-first #(= val (second %)) grid-map)))

(defn coord-neighbors [c]
  (let [[x y] c
        x+ (inc x)
        x- (dec x)
        y+ (inc y)
        y- (dec y)]
    #{[x y] [x+ y] [x- y] [x y+] [x y-]}))

(defn expand-coords [coords]
  (into #{} (mapcat coord-neighbors coords)))

(defn expand-region-coords [grid-map coords]
  (filter
   #(= \1 (get grid-map %))
   (expand-coords coords)))

(defn find-coords-in-region [grid-map init-coord]
  (loop [coords-in-region [init-coord]]
    (let [expanded-coords-in-region (expand-region-coords grid-map coords-in-region)]
      (if (= expanded-coords-in-region coords-in-region)
        coords-in-region
        (recur expanded-coords-in-region )))))

(defn mark-region [new-val coords grid-map]
  (reduce (fn [m c] (assoc m c new-val)) grid-map coords))

(defn find-and-mark-region [grid-map id]
  (if-let [init-coord (find-coord \1 grid-map)]
    (let [region-coords (find-coords-in-region grid-map init-coord)]
      (mark-region id region-coords grid-map))))

(defn count-regions [grid-map]
  (loop [temp-grid grid-map
         n 0
         region-id 1]
    (let [updated-grid (find-and-mark-region temp-grid region-id)]
      (if updated-grid
        (recur updated-grid (inc n) (inc region-id))
        n))))

(defn index-line [[line-index line]]
  (mapv (fn [el-index val] [[line-index el-index] val]) (range) line))

(defn into-coord-map [grid]
  (->> grid
       (map vector (range))
       (mapcat index-line)
       (into {})))

(defn solve-1 [input]
  (->> input
       (hashes 128)
       (apply str)
       (count-ones)))

(defn solve-2 [input]
  (->> input
       (hashes 128)
       (into-coord-map)
       (count-regions)))

(defn solve-day-14 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
