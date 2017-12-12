(ns advent-of-code.day10 (:require [advent-of-code.main :refer :all]))

(def init-list (apply vector (range 256)))

(def init-state {:vals init-list :val-count (count init-list) :pos 0 :skip-size 0})

(defn move-forward [state length]
  (let [skip-size (:skip-size state)
        wrap (fn [x] (mod x (:val-count state)))]
    (-> state
        (update :pos #(wrap (+ % skip-size length)))
        (update :skip-size inc))))

(defn reverse-sublist [coll skip len]
  (let [coll-len (count coll)
        _ (assert (<= len coll-len) "can't reverse a sublist longer than the list")
        [to-reverse rest-cycle] (split-at len (drop skip (cycle coll)))
        reversed (reverse to-reverse)
        rest-list (take (- coll-len len) rest-cycle)
        new-list (concat reversed rest-list)
        [new-end new-start] (split-at (- coll-len skip) new-list)]
    (concat new-start new-end)))

(defn string->ints [s]
  (mapv #(Integer/parseInt %) (clojure.string/split s #"[,\n]")))

(defn step [state length]
  (let [vals (:vals state)
        pos (:pos state)]
    (-> state
        (update :vals #(reverse-sublist % pos length))
        (move-forward length))))

(defn solve-1 [input]
  (->> input
       (string->ints)
       (reduce step init-state)
       (:vals)
       (take 2)
       (reduce * 1)))

(defn solve-2 [input]
  (->> input
       (.trim)
       (map int)
       (flip concat [17, 31, 73, 47, 23])
       (repeat 64)
       (flatten)
       (reduce step init-state)
       (:vals)
       (partition 16)
       (map (partial apply bit-xor))
       (trace)
       (map #(format "%02x" %))
       (reduce str)))

(defn solve-day-10 []
  (let [input (load-input-file "10")]
    (clojure.pprint/pprint (solve-1 input))
    (clojure.pprint/pprint (solve-2 input))))
