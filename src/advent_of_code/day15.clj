(ns advent-of-code.day15 (:require [advent-of-code.main :refer :all]))

(def divisor 2147483647)
(def gen-a {:factor 16807 :divisor divisor :start-val 699 :filter-val 4})
(def gen-b {:factor 48271 :divisor divisor :start-val 124 :filter-val 8})

(def part-1-amount (* 40 1000 1000))
(def part-2-amount (* 5 1000 1000))

(defn gen-next [gen prev-val]
  (let [{factor :factor divisor :divisor} gen]
    (mod (* prev-val factor) divisor)))

(defn divides [a b] (= 0 (mod b a)))

(defn gen-range-1 [gen] (iterate #(gen-next gen %) (:start-val gen)))

(defn gen-range-2 [gen] (filter #(divides (:filter-val gen) %) (gen-range-1 gen)))

(defn dec->bin [x] (clojure.pprint/cl-format nil "~b" x))

(defn to-32-bits [x] (zp (dec->bin x) 32))

(defn ints->last-16-bits [pair]
  (mapv #(bit-and % 0xFFFF) pair))

(defn count-elements-with-equal-last-16-bits [pairs]
  (->> pairs
       (map ints->last-16-bits)
       (filter (partial apply =))
       (count)))

(defn solve-1 []
  (->> (map vector (gen-range-1 gen-a) (gen-range-1 gen-b))
       (take part-1-amount)
       (count-elements-with-equal-last-16-bits)))

(defn solve-2 []
  (->> (map vector (gen-range-2 gen-a) (gen-range-2 gen-b))
       (take part-2-amount)
       (count-elements-with-equal-last-16-bits)))

(defn solve-day-15 []
  (clojure.pprint/pprint (solve-1))
  (clojure.pprint/pprint (solve-2)))
