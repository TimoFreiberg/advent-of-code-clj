(ns advent-of-code.day18 (:require [advent-of-code.main :refer :all]))

(defn get-val [state x]
  (cond
    (keyword? x) (get-in state [:reg x] 0)
    (number? x) x
    :else (do
            (println "WARN: can't resolve" x)
            nil)))

(defn set-val [state reg val]
  (assoc-in state [:reg reg] val))

(defn apply-cursor [state f]
  (update state :cursor f))

(defn inc-cursor [state]
  (apply-cursor state inc))

(defn apply-fn-to-reg [state x y op]
  (let [x-val (get-val state x)
        y-val (get-val state y)]
    (trace "storing (" op x-val y-val ") in" x)
    (set-val state x (op x-val y-val))))

(defn recover-val [state]
  (get state :snd))

(defn cmd-sound [state x]
  (let [x-val (get-val state x)]
    (trace "sound:" x "/" x-val)
    (inc-cursor
     (assoc state :snd x-val))))

(defn cmd-send [state x]
   )

(defn cmd-set [state x y]
  (let [y-val (get-val state y)]
    (trace "set" x "to" y-val)
    (inc-cursor
     (set-val state x y-val))))

(defn cmd-add [state x y]
  (inc-cursor
   (apply-fn-to-reg state x y +)))

(defn cmd-mul [state x y]
  (inc-cursor
   (apply-fn-to-reg state x y *)))

(defn cmd-mod [state x y]
  (inc-cursor
   (apply-fn-to-reg state x y mod)))


(defn cmd-recover [state x]
  (let [recovered (recover-val state)]
    (if (not= 0 (get-val state x))
      (trace "recover" x "val" recover-val)
      (trace "don't recover" x "because its val is 0"))
    (inc-cursor state)))

(defn cmd-jgz [state x y]
  (let [y-val (get-val state y)]
    (if (> (get-val state x) 0)
      (do (trace "jumping" y-val)
          (apply-cursor state #(+ % (get-val state y))))
      (do
        (trace "not jumping, because" x "has val 0")
        (inc-cursor state)))))

(defn cursor-in-range [state]
  (trace "getting cursor from" state)
  (let [cursor (:cursor state)]
    (and
     (>= cursor 0)
     (< cursor (:instruction-count state)))))

(defn mk-init-state [instructions]
  {:instruction-count (count instructions)
   :cursor 0
   :iterations 0})

(defn apply-instruction [instruction state]
  (let [{op :op arg-1 :arg-1 arg-2 :arg-2} instruction]
    (trace "instruction:" instruction)
    (update
     (if (contains? instruction :arg-2)
       (do
         (trace "op with 2 args:" op arg-1 arg-2)
         (op state arg-1 arg-2))
       (do
         (trace "op with 1 arg:" op arg-1)
         (op state arg-1)))
     :iterations inc)))

(defn run-program [instructions]
  (trace "running program with instructions:" instructions)
  (take-while
   #(cursor-in-range (second %))
   (iterate
    (fn ap-instr [[_ state]]
      (let [instruction (get instructions (get state :cursor))]
        [instruction
         (apply-instruction instruction state)]))
    [nil (mk-init-state instructions)])))

(defn run-n-programs)

(defn keyword-or-number [s]
  (if (nil? s)
    nil
    (if (not-empty (re-find #"\d+" s))
      (Integer/parseInt s)
      (keyword s))))

(defn parse-instruction-1 [line]
  (let [[cmd arg-1 arg-2] (split-words line)
        arg-1 (keyword arg-1)
        arg-2 (keyword-or-number arg-2)]
    (case cmd
      "snd" {:op cmd-sound :arg-1 arg-1}
      "set" {:op cmd-set :arg-1 arg-1 :arg-2 arg-2}
      "add" {:op cmd-add :arg-1 arg-1 :arg-2 arg-2}
      "mul" {:op cmd-mul :arg-1 arg-1 :arg-2 arg-2}
      "mod" {:op cmd-mod :arg-1 arg-1 :arg-2 arg-2}
      "rcv" {:op cmd-recover :arg-1 arg-1}
      "jgz" {:op cmd-jgz :arg-1 arg-1 :arg-2 arg-2})))

(defn parse-instruction-2 [line]
  (let [[cmd arg-1 arg-2] (split-words line)
        arg-1 (keyword arg-1)
        arg-2 (keyword-or-number arg-2)]
    (case cmd
      "snd" {:op cmd-send :arg-1 arg-1}
      "set" {:op cmd-set :arg-1 arg-1 :arg-2 arg-2}
      "add" {:op cmd-add :arg-1 arg-1 :arg-2 arg-2}
      "mul" {:op cmd-mul :arg-1 arg-1 :arg-2 arg-2}
      "mod" {:op cmd-mod :arg-1 arg-1 :arg-2 arg-2}
      "rcv" {:op cmd-recover :arg-1 arg-1}
      "jgz" {:op cmd-jgz :arg-1 arg-1 :arg-2 arg-2})))

(defn parse-input [s]
  (->> s
       (clojure.string/split-lines)
       (mapv parse-instruction)))

(defn op-is [op log-entry]
  (= op (get-in log-entry [0 :op])))

(defn arg-val-of-op-is [n log-entry]
  (let [[{arg :arg-1} state] log-entry
        arg-val (get-val state arg)]
    (= n arg-val)))

(defn solve-1 [steps]
  (let [prog-log (->> steps
                      (parse-input)
                      (run-program))]
    (->> prog-log
         (filter #(op-is cmd-recover %))
         (filter #(not (arg-val-of-op-is 0 %)))
         (first))))

(defn solve-2 [steps])

(def input (load-input-file "18"))
(def parsed-input (parse-input input))

(defn solve-day-18 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
