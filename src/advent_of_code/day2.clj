(ns advent-of-code.day2 (:require [advent-of-code.main :refer [load-input-file]]))

(defn greatest-diff [coll]
  (- (apply max coll) (apply min coll)))

(defn divs [a b]
  (and (not= a b)
       (= 0
          (mod a b))))

(defn list-pairs [coll]
  (for [x coll y coll]
    [x y]))

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn divs-one-in [coll x]
  (filter #(divs x %) coll))

(defn div-pair? [[a b]]
  (divs a b))

(defn find-divisible-pair [coll]
  (->> coll
       (list-pairs)
       (filter div-pair?)
       (first)))

(defn only-divisible-quotient [coll]
  (let [[a b] (find-divisible-pair coll)]
    (/ a b)))

(defn string->ints [string]
  (let [split (clojure.string/split string #"\s+")]
    (mapv #(Integer/parseInt %) split)))

(defn solve-1 [input]
  (->> input
       (clojure.string/split-lines)
       (map string->ints)
       (map greatest-diff)
       (reduce +)))

(defn solve-2 [input]
  (->> input
       (clojure.string/split-lines)
       (map string->ints)
       (map only-divisible-quotient)
       (reduce +)))

(defn solve-day-2 []
  (let [input (load-input-file "2")]
    (println (solve-1 input))
    (println (solve-2 input))))

