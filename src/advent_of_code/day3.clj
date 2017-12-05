(ns advent-of-code.day3 (:require [advent-of-code.main :refer [load-input-file find-first]]))

(defn move-field [field dir]
  (let [[pos by]
        (case dir
          :left [:x dec]
          :right [:x inc]
          :up [:y inc]
          :down [:y dec])]
    (-> field
        (update pos by)
        (update :val inc))))

(defn gen-side-fields [side-length start-field dir]
  (let [fields (take side-length (drop 1 (iterate #(move-field % dir) start-field)))]
    [(last fields) fields]))

(defn run-spiral [field side-length]
  (let [bot-right-field (move-field field :right)
        [top-right-field right-fields] (gen-side-fields
                                        (dec side-length) bot-right-field :up)
        [top-left-field top-fields] (gen-side-fields
                                     side-length top-right-field :left)
        [bot-left-field left-fields] (gen-side-fields
                                      side-length top-left-field :down)
        [bot-right-field-2 bot-fields] (gen-side-fields
                                        side-length bot-left-field :right)]
    [bot-right-field-2
     (concat [bot-right-field] right-fields top-fields left-fields bot-fields)]))

(def spiral (lazy-seq
             (let [start {:x 0 :y 0 :val 1}]
               (mapcat second
                       (reductions
                        (fn
                          [[field _] side-length]
                          (let [[last-field new-fields] (run-spiral field side-length)]
                            [last-field new-fields]))
                        [start [start]] (iterate #(+ 2 %) 2))))))

(defn neighbor-coords [x y]
  (filter #(not= % [x y])
          (for [i (range (dec x) (+ x 2)) j (range (dec y) (+ y 2))]
            [i j])))

(defn coords-match [x0 y0 {x1 :x y1 :y}]
  (and (= x0 x1) (= y0 y1)))

(defn find-field-at [fields [x y]]
  (find-first #(coords-match x y %) fields))

(defn trace [s]
  (println s)
  s)

(defn sum-of-neighbors [fields x y]
  (->> (neighbor-coords x y)
       (map #(find-field-at fields %))
       (filter identity)
       (map :val)
       (reduce +)))

(defn add-field-with-neighbor-sum-val [[fields _] {x :x y :y :as new-field}]
  (let [updated-field (assoc new-field :val (sum-of-neighbors fields x y))]
    [(conj fields updated-field) updated-field]))

(def spiral-with-neighbor-sum-vals
  (let [first-field (first spiral)]
    (map second (reductions add-field-with-neighbor-sum-val [[first-field] first-field] (rest spiral)))))


(defn add-coords [{x :x y :y}]
  (+ (Math/abs x) (Math/abs y)))

(defn solve-1 [input]
  (->> spiral
       (drop (dec input))
       (first)
       (add-coords)))

(defn solve-2 [input]
  (->> spiral-with-neighbor-sum-vals
       (map :val)
       (drop-while #(>= input %))
       (first)
       ))

(defn solve-day-3 []
  (let [input 347991]
    (println (solve-1 input))
    (println (solve-2 input))))
