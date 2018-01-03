(ns advent-of-code.day20 (:require
                          [advent-of-code.main :refer :all]
                          [instaparse.core :as insta]))

(def line-parser
  (insta/parser
   "<line> = trim pos comma vel comma accel trim
<trim> = <#'\\s*'>
<comma> = trim <','> trim
pos = <'p=<'> num comma num comma num <'>'>
vel = <'v=<'> num comma num comma num <'>'>
accel = <'a=<'> num comma num comma num <'>'>
<num> = trim #'(-)?\\d+' trim"
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
  (let [new-vel (+vec accel vel)]
    (-> particle
        (update :pos +vec new-vel)
        (assoc :vel new-vel))))

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

(defn remove-colliding-particles [particles]
  (let [duplicate-positions (into #{}
                                  (for [[id freq] (frequencies (mapv :pos particles))
                                        :when (> freq 1)]
                                    id))]
    (filter #(not (contains? duplicate-positions (:pos %))) particles)))

(defn run-simulation-tick [state]
  (->> state
       (mapv tick)
       (remove-colliding-particles)))

(defn resolve-collisions [particles]
  (loop [state (remove-colliding-particles particles)
         particles-count (count particles)
         counter 0
         last-collision 0]
    (let [next-state (run-simulation-tick state)
          next-particles-count (count next-state)
          print-state #(trace
                        "Iteration"
                        counter
                        ", particles left:"
                        particles-count)
          mk-report (fn [] {:state state
                            :particles-left particles-count
                            :iterations counter})]
      (trace (mk-report))
      (cond

        (zero? next-particles-count) (do
                                       (trace "No particles left")
                                       (mk-report))

        (not=
         next-particles-count particles-count)
        (do
          (trace "Collision at iteration " counter)
          (print-state)
          (recur next-state next-particles-count (inc counter) counter))

        (> (- counter last-collision) 500) (mk-report)

        :else (do (when (zero? (mod counter 1000)) (print-state))
                  (recur
                   next-state
                   next-particles-count
                   (inc counter)
                   last-collision))))))

(defn solve-2 [input]
  (-> input
      (parse-input)
      (resolve-collisions)
      (:state)
      (count)))

(def input (load-input-file "20"))

(defn solve-day-20 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
