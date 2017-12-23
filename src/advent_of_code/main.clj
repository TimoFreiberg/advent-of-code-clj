(ns advent-of-code.main)

(defn find-first [pred coll]
  (first (filter pred coll)))

(def ^:dynamic *verbose* false)

(defn trace [& things]
  (when *verbose*
    (clojure.pprint/pprint things))
  (last things))

(defn trace-pprint [& things]
  (when *verbose*
    (run! clojure.pprint/pprint things))
  (last things))

(defmacro verbosely [& rest]
  `(binding [*verbose* true] ~@rest))

(defn split-words [s] (clojure.string/split s #"[\n\s]+"))

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

