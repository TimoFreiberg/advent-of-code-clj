(ns advent-of-code.main)

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn trace [& things]
  (doseq [s things]
    (clojure.pprint/pprint s))
  (last things))

(defn split-words [s] (clojure.string/split s #"\s+"))

(defn string-to-int [s]
  (->> s
       (filter #(Character/isDigit %))
       (apply str)
       (Integer/parseInt)))

(defn zp "Zero Pad numbers - takes a number and the length to pad to as arguments"
  [n c]
  (loop [s (str n)]
    (if (< (.length s) c)
      (recur (str "0" s))
      s)))

(defn flip [f & args]
  (apply f (reverse args)))

(defn load-input-file "load-input-file [day-number] loads content of the input file for the given day " [day-number]
  (slurp (str "resources/day" day-number)))
