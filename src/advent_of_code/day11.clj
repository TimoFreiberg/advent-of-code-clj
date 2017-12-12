(ns advent-of-code.day11 (:require [advent-of-code.main :refer :all]))

(def dirs [:n :ne :se :s :sw :nw])
(def init-state (-> (zipmap dirs (repeat 0))))

(def simplifiable-dirs
  [[[:n :se] :ne]
   [[:ne :s] :se]
   [[:se :sw] :s]
   [[:s :nw] :sw]
   [[:sw :n] :nw]
   [[:nw :ne] :n]])

(defn step [state dir]
  (-> state
      (update dir inc)))

(defn direction? [s]
  (.contains dirs s))

(defn cancel-vals
  ([[k1 k2] m]
   (cancel-vals [k1 k2] nil m))
  ([[k1 k2] into-k m]
   (let [v-min (min (get m k1) (get m k2))
         cancelled (-> m
                       (update k1 #(- % v-min))
                       (update k2 #(- % v-min)))]
     (if into-k
       (update cancelled into-k #(+ % v-min))
       cancelled))))

(defn simplify [[k1 k2] k-to m]
  (let [v-min (min (get m k1) (get m k2))]
    (-> m
        (update k1 #((fnil - 0) % v-min))
        (update k2 #((fnil - 0) % v-min))
        (update k-to #((fnil + 0) % v-min)))))

(defn simplify-neighboring [dir-map]
  (reduce
   (fn [m [[k1 k2] k3]] (cancel-vals [k1 k2] k3 m))
   dir-map
   simplifiable-dirs))

(defn negate-opposing [dir-map]
  (->> dir-map
       (cancel-vals [:s :n])
       (cancel-vals [:ne :sw])
       (cancel-vals [:se :nw])))

(defn solve-both [input]
  (->> input
       (flip clojure.string/split #"[,\n]")
       (map keyword)
       (filter direction?)
       (reductions step init-state)
       (map negate-opposing)
       (map simplify-neighboring)))

(defn solve-1 [input]
  (->> input
       (solve-both)
       (last)
       (vals)
       (trace :solved-1)
       (apply +)))
  ;; (->> input
  ;;      (flip clojure.string/split #"[,\n]")
  ;;      (map keyword)
  ;;      (filter direction?)
  ;;      (reduce step init-state)
  ;;      (negate-opposing)
  ;;      (simplify-neighboring)
  ;;      (trace)
  ;;      (vals)
  ;;      (apply +)))

(defn solve-2 [input]
  (->> input
       (solve-both)
       (apply max-key #(apply + (vals %)))
       (trace :solved-2)
       (vals)
       (apply +)))

(defn solve-day-11 []
  (let [input (load-input-file "11")]
    (clojure.pprint/pprint (solve-1 input))
    (clojure.pprint/pprint (solve-2 input))))
