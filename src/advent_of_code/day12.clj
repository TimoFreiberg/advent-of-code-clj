(ns advent-of-code.day12 (:require [advent-of-code.main :refer :all]))

(defn parse-line [s]
  (let [[id targets] (clojure.string/split s #"<->")]
    [(.trim id) (mapv #(.trim %) (clojure.string/split targets #","))]))

(defn collect-all-in-group [id coll]
  (loop [ids #{id}]
    (let [connected-ids (into #{}
                              (concat ids
                                      (mapcat (partial get coll) ids)))]
      (if (= ids connected-ids)
        connected-ids
        (recur connected-ids)))))

(defn solve-1 [input]
  (->> input
       (clojure.string/split-lines)
       (mapv parse-line)
       (into {})
       (collect-all-in-group "0")
       (count)))

(defn count-groups [input]
  (reduce
   (fn [groups id] (if (some identity (map #(.contains % id) groups))
                     groups
                     (conj groups (collect-all-in-group id input))))
   #{} (keys input)))

(defn solve-2 [input]
  (->> input
       (clojure.string/split-lines)
       (mapv parse-line)
       (into {})
       (count-groups)
       (count)))


(defn solve-day-12 []
  (let [input (load-input-file "12")]
    (clojure.pprint/pprint (solve-1 input))
    (clojure.pprint/pprint (solve-2 input))))
