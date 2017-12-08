(ns advent-of-code.day7 (:require [advent-of-code.main :refer :all]))

(defn split-commas [s] (clojure.string/split s #","))

(defn parse-line [line]
  (let [[base children] (clojure.string/split line #"->")
        [name weight] (split-words base)
        children-vec (if children
                       (mapv clojure.string/trim
                             ((fnil split-commas "") children)))]
    {:name name :weight (string-to-int weight) :children children-vec}))

(defn lookup-node [tree name]
  (first (filter #(= name (:name %)) tree)))

(defn sum-weight [tree node]
  ((fnil + 0 0) (:weight node)
                (->> (:children node)
                     (map #(lookup-node tree %))
                     (map #(sum-weight tree %))
                     (reduce + 0))))

(defn has-child [node name]
  (if-let [children (:children node)]
    (.contains children name)))

(defn all-equal [xs]
  (if-not (empty? xs)
    (= 1 (count (into #{} xs)))
    true))

(defn get-children [tree node]
  (->> (:children node)
       (mapv #(lookup-node tree %))))

(defn has-balanced-children [node tree]
  (->> (get-children tree node)
       (map :sum-weight)
       (all-equal)))

(defn has-no-children [node]
  (empty? (:children node)))

(defn enrich-tree-with-sum-weight [tree]
  (map #(assoc % :sum-weight (sum-weight tree %)) tree))

(defn find-mode-by [f coll]
  (let [outlier-key (->> (mapv f coll)
                         (frequencies)
                         (apply max-key second)
                         (first))]
    (find-first #(= outlier-key (f %)) coll)))

(defn find-outlier-by [f coll]
  (let [outlier-key (->> (mapv f coll)
                         (frequencies)
                         (apply min-key second)
                         (first))]
    (find-first #(= outlier-key (f %)) coll)))

(defn find-leafmost-node-with-unbalanced-children [tree]
  (let [root (find-root tree)]
    (loop [node root]
      (cond
        (has-no-children node) :tree-balanced
        (has-balanced-children node tree) node
        :else (recur (find-outlier-by :sum-weight (get-children tree node)))))))

(defn find-parent [node tree]
  (let [possible-parents (filter #(has-child % (:name node)) tree)]
    (first possible-parents)))

(defn find-root [tree]
  (loop [node (first tree)]
    (let [parent (find-parent node tree)]
      (if parent
        (recur parent)
        node))))

(defn make-tree [input-string]
  (->> input-string
       (clojure.string/split-lines)
       (mapv parse-line)
       (enrich-tree-with-sum-weight)))

(defn solve-1 [input]
  (->> input
       (make-tree)
       (find-root)))

(defn solve-2 [input]
  (let [tree (make-tree input)
        target-node (find-leafmost-node-with-unbalanced-children tree)
        target-parent (find-parent target-node tree)
        target-siblings (get-children tree target-parent)
        common-weight (:sum-weight (find-mode-by :sum-weight target-siblings))
        wrong-weight (:sum-weight target-node)
        weight-diff (- wrong-weight common-weight)
        correct-single-weight (- (:weight target-node) weight-diff)]
    (println "correct weight: " correct-single-weight)))

(defn solve-day-7 []
  (let [input (load-input-file "7")]
    (clojure.pprint/pprint (solve-1 input))
    (clojure.pprint/pprint (solve-2 input))))
