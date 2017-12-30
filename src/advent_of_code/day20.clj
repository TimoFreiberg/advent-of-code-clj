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
      (mapv parse-line)))

(defn +vec [vec1 vec2]
  (mapv + vec1 vec2))

(defn tick [{:keys [pos vel accel] :as particle}]
  (-> particle
      (update :pos +vec vel)
      (update :vel +vec accel)))

(defn manhattan-distance [{:keys [pos]}]
  (->> pos
       (mapv #(Math/abs %))
       (reduce + 0)))

(defn solve-1 [input]
  )

(defn solve-2 [input]
  )

(def input (load-input-file "20"))

(defn solve-day-20 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
