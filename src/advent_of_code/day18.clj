(ns advent-of-code.day18 (:require
                          [advent-of-code.main :refer :all]))

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
        y-val (get-val state y)
        new-val (op x-val y-val)]
    (trace "storing" new-val "in" x)
    (set-val state x (op x-val y-val))))

(defn cmd-sound [state x]
  (let [x-val (get-val state x)]
    (trace "sound:" x "/" x-val)
    (inc-cursor
     (assoc state :snd x-val))))

(defn cmd-send [state x]
  (let [x-val (get-val state x)]
    (.add (:queue-out state) x-val)
    (inc-cursor state)))

(defn cmd-receive [state x]
  (let [queue (:queue-in state)]
    (if (.isEmpty queue)
      (assoc state :blocked true)
      (-> state
          (set-val x (.remove queue))
          (dissoc :blocked)
          (inc-cursor)))))

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

(defn cmd-jgz [state x y]
  (let [y-val (get-val state y)]
    (if (> (get-val state x) 0)
      (do (trace "jumping" y-val)
          (apply-cursor state #(+ % (get-val state y))))
      (do
        (trace "not jumping, because" x "has val 0")
        (inc-cursor state)))))

(defn cursor-in-range [state]
  (let [cursor (:cursor state)]
    (and
     (>= cursor 0)
     (< cursor (:instruction-count state)))))

(defn mk-init-state [instructions]
  {:instruction-count (count instructions)
   :cursor 0})

(defn apply-instruction-common [instruction state]
  (let [{:keys [op arg-1 arg-2]} instruction
        cursor (:cursor state)
        state (inc-cursor state)
        get-val (partial get-val state)]
    (case op
      :set (set-val state arg-1 (get-val arg-2))
      :add (set-val state arg-1 (+
                                 (get-val arg-1)
                                 (get-val arg-2)))
      :mul (set-val state arg-1 (*
                                 (get-val arg-1)
                                 (get-val arg-2)))
      :mod (set-val state arg-1 (mod
                                 (get-val arg-1)
                                 (get-val arg-2)))
      :jgz (if (pos? (get-val arg-1))
             (assoc state :cursor (+
                                   cursor
                                   (get-val arg-2)))
             state)
      (do
        (println "could not apply instruction" op)
        state))))

(defn apply-instruction-1 [instruction state]
  (let [{:keys [op arg-1 arg-2]} instruction
        cursor (:cursor state)
        _ (if (nil? (:cursor state)) (prn state))
        state (update state :cursor inc)
        get-val (partial get-val state)]
    (case op
      :snd (assoc state :snd (get-val arg-1))
      :rcv (if (pos? (get-val arg-1))
             (assoc state :recovered (:snd state))
             state)
      (apply-instruction-common instruction state))))

(defn both-blocked? [[log-entry-1 log-entry-2]]
  (and
   (get-in log-entry-1 [1 :blocked])
   (get-in log-entry-2 [1 :blocked])))

(defn keyword-or-number [s]
  (if (nil? s)
    nil
    (if (not-empty (re-find #"\d+" s))
      (Integer/parseInt s)
      (keyword s))))

(defn parse-instruction [line]
  (let [[cmd arg-1 arg-2] (mapv keyword-or-number (split-words line))]
    {:op cmd :arg-1 arg-1 :arg-2 arg-2}))

(defn parse-input [s]
  (->> s
       (clojure.string/split-lines)
       (mapv parse-instruction)))

(defn solve-1 [steps]
  (let [instructions (parse-input steps)]
    (loop [state (mk-init-state instructions)]
      (let [recovered (:recovered state)
            cursor (:cursor state)]
        (if recovered
          recovered
          (recur (apply-instruction-1
                  (get instructions (:cursor state))
                  state)))))))

(defn solve-2 [steps])

(def input (load-input-file "18"))

(defn solve-day-18 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
