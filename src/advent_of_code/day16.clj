(ns advent-of-code.day16 (:require [advent-of-code.main :refer :all]))

(def alphabetical-names "abcdefghijklmnop")

(defn mk-init-state [names]
  (into {}
        (map vector
             (range)
             (map (comp keyword str) names))))

(def max-index 15)

(defn map-invert
  [m] (reduce (fn [m [k v]] (assoc m v k)) {} m))

(defn vals-to-indexes [m]
  (if (keyword? (first m))
    m
    (map-invert m)))

(defn indexes-to-vals [m]
  (if (number? (first m))
    m
    (map-invert m)))

(defn inc-vals [m]
  (clojure.pprint/pprint m)
  (reduce
   (fn [m' k] (update m' k inc))
   m
   (keys m)))

(defn find-first-key-by-val [m v]
  (->> m
       (filter (fn [[_ v?]] (= v v?)))
       (first)
       (first)))

(defn do-spin [m]
  (let [m' (vals-to-indexes m)
        last-val (find-first-key-by-val m' max-index)]
    (-> m'
        (inc-vals)
        (dissoc last-val)
        (assoc last-val 0))))

(defn spin [m x]
  (loop [m m x x]
    (if (= 0 x)
      m
      (recur (do-spin m) (dec x)))))

(defn exchange [m x y]
  (let [m (indexes-to-vals m)
        x-val (get m x)]
    (-> m
        (assoc x (get m y))
        (assoc y x-val))))

(defn partner [m a b]
  (let [m (vals-to-indexes m)
        a-pos (get m a)]
    (-> m
        (assoc a (get m b))
        (assoc b a-pos))))

(defn parse-instruction [s]
  (let [[instruction-char params] (mapv (partial apply str) (split-at 1 s))]
    (case instruction-char
      "s" (let [times (Integer/parseInt params)]
            (fn spin' [m] (spin m times)))
      "x" (let [[x y] (clojure.string/split params #"/")
                x (Integer/parseInt x)
                y (Integer/parseInt y)]
            (fn exchange' [m] (exchange m x y)))
      "p" (let [[a b] (clojure.string/split params #"/")
                a (keyword a)
                b (keyword b)]
            (fn partner' [m] (partner m a b)))
      (do (prn "can't parse" s ", instruction key:" instruction-char) identity))))

(defn parse-input [s]
  (map parse-instruction (clojure.string/split s #"[,\n]")))

(defn perform-dance
  ([moves] (perform-dance alphabetical-names moves))
  ([init-names moves]
   (let [end-state (reduce
                    (fn [state op] (op state))
                    (mk-init-state init-names)
                    moves)]
     (->> end-state
          (indexes-to-vals)
          (filter (comp number? first))
          (sort-by first)
          (mapv second)
          (mapv name)
          (apply str)))))

(defn dance-until-loop [moves]
  (loop [n 0
         last-pos (mk-init-state alphabetical-names)
         positions #{(apply str (mapv name alphabetical-names))}]
    (let [next-pos (perform-dance moves)]
      (if (contains? positions next-pos)
        {:iterations n :last-pos last-pos}
        (recur (inc n) next-pos (conj positions next-pos))))))

(defn solve-1 [input]
  (->> input
       (parse-input)
       (perform-dance)))

(defn solve-2 [input]
  (->> input
       (parse-input)
       (dance-until-loop)))

(def input (load-input-file "16"))

(defn solve-day-16 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
