(ns advent-of-code.day8 (:require [advent-of-code.main :refer :all]))

(defn parse-op [s]
  (case s
    "inc" +
    "dec" -
    (do (println "could not parse op" s)
        identity)))

(defn parse-cond-op [s]
  (case s
    ">" >
    "<" <
    ">=" >=
    "<=" <=
    "!=" not=
    "==" =
    (do (println "could not parse cond-op" s)
        identity)))

(defn pairs [as bs]
  (mapcat
   (fn [a] (map
            (fn [b] (str a b))
            bs))
   as))

(defn parse-instr [s]
  (let [[op-target op amount const-if cond-target cond-op cond-val] (split-words s)]
    (assert (and
             (= const-if "if")
             op-target op amount cond-target cond-op cond-val))
    {:op-target op-target :op (parse-op op) :amount (Integer/parseInt amount) :cond-target cond-target :cond-op (parse-cond-op cond-op) :cond-val (Integer/parseInt cond-val)}))

(defn apply-instruction
  [values
   {op-target :op-target
    op :op
    amount :amount
    cond-target :cond-target
    cond-op :cond-op
    cond-val :cond-val
    :as instruction}
   ]
  (let [actual-cond-val (or (get values cond-target) 0)
        actual-op-val (or (get values op-target) 0)
        new-val (if (cond-op actual-cond-val cond-val)
                  (op actual-op-val amount)
                  actual-op-val)
        highest-val (max new-val (or (:highest values) new-val))]
    (-> values
        (assoc op-target new-val)
        (assoc :highest highest-val))))

(defn get-register-names [coll]
  (concat (into #{} (mapv :op-target coll)) (into #{} (mapv :cond-target coll))))

(defn solve-1 [input]
  (let [instructions (->> input
                          (clojure.string/split-lines)
                          (mapv parse-instr))]
    (->> instructions
         (reduce apply-instruction {})
         (apply max-key second))))

(defn solve-2 [input]
  (let [instructions (->> input
                          (clojure.string/split-lines)
                          (mapv parse-instr))]
    (->> instructions
         (reduce apply-instruction {})
         (:highest))))

(defn solve-day-8 []
  (let [input (load-input-file "8")]
    (clojure.pprint/pprint (solve-1 input))
    (clojure.pprint/pprint (solve-2 input))))
