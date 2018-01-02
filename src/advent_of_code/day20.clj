(ns advent-of-code.day20 (:require
                          [advent-of-code.main :refer :all]
                          [instaparse.core :as insta]))

(def line-parser
  (insta/parser
   "<line> = pos comma vel comma accel
<comma> = <',' #'\\s*'>
pos = <'p=<'> num comma num comma num <'>'>
vel = <'v=<'> num comma num comma num <'>'>
accel = <'a=<'> num comma num comma num <'>'>
<num> = #'(-)?\\d+'"
   :output-format :enlive))

(defn parse-line [line]
  (let [parsed (line-parser line)
        mk-vec (fn [xs] (mapv #(Integer/parseInt %) xs))
        [pos vel accel] (mapv (comp mk-vec :content) parsed)]
    {:pos pos :vel vel :accel accel}))

(defn parse-input [input]
  (->> input
       (clojure.string/split-lines)
       (mapv parse-line)
       (mapv #(assoc %2 :ix %1) (range))))

(defn +vec [vec1 vec2]
  (mapv + vec1 vec2))

(defn tick [{:keys [pos vel accel] :as particle}]
  (-> particle
      (update :pos +vec vel)
      (update :vel +vec accel)))

(defn manhattan-distance [vec]
  (->> vec
       (mapv #(Math/abs %))
       (reduce + 0)))

(defn by-accel-then-vel [a b]
  (let [compare-by-keys (fn [particle]
                          (mapv
                           #(manhattan-distance (% particle))
                           [:accel :vel :pos]))]
    (compare (compare-by-keys a) (compare-by-keys b))))

(defn retain-x-with-zero-y [p x-key y-key]
  (update
   p x-key
   (fn [x]
     (->> x
          (map vector (y-key p))
          (filter #(= 0 (first %)))
          (map second)))))

(defn find-particle-with-lowest-acceleration [particles]
  (let [accel-total (comp manhattan-distance :accel)
        min-accel (apply min (map accel-total particles))
        min-accel-particles (filter #(= min-accel (accel-total %)) particles)
        static-vel-particles (map
                              #(retain-x-with-zero-y % :vel :accel)
                              min-accel-particles)
        static-pos-particles (map
                              #(retain-x-with-zero-y % :pos :vel)
                              static-vel-particles)]
    (->> static-pos-particles
         (sort by-accel-then-vel)
         (first)
         (:ix))))

(defn solve-1 [input]
  (->> input
       (parse-input)
       (find-particle-with-lowest-acceleration)))

(defn find-duplicate-positions [particles]
  (loop [stack particles
         found-positions #{}
         duplicate-positions #{}]
    (if (empty? stack)
      duplicate-positions
      (let [current-pos (:pos (first stack))]
        (if (contains? found-positions current-pos)
          (recur
           (rest stack)
           found-positions
           (conj duplicate-positions current-pos))
          (recur
           (rest stack)
           (conj found-positions current-pos)
           duplicate-positions))))))

(defn remove-colliding-particles [particles]
  (let [duplicate-positions (into #{}
                                  (for [[id freq] (frequencies (mapv :pos particles))
                                        :when (> freq 1)]
                                    id))]
    (filter #(not (contains? duplicate-positions (:pos %))) particles)))

(defn run-simulation-tick [state]
  (-> state
      (update :particles
              #(->> %
                    (mapv tick)
                    (remove-colliding-particles)))
      (update :iterations inc)))

(defn resolve-collisions [particles]
  (loop [state particles
         particles-count (count particles)
         counter 0
         last-collision 0]
    (let [next-state (run-simulation-tick state)
          next-particles-count (count next-state)]
      (if (not= next-particles-count particles-count)
        (do
          (println "Collision at iteration " counter)
          (recur next-state next-particles-count (inc counter) counter))
        (if (> (- counter last-collision) 5000)
          {:state state
           :particles-left particles-count
           :iterations counter}
          (recur next-state next-particles-count (inc counter) last-collision))))))

(defn solve-2 [input]
  (->> input
       (parse-input)
       (resolve-collisions)))

(def input (load-input-file "20"))

(defn solve-day-20 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
