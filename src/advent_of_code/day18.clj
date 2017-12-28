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

(defn mk-init-state
  ([instructions] (mk-init-state instructions 0))
  ([instructions prog-id] {:instruction-count (count instructions)
                           :cursor 0
                           :reg {:p prog-id}
                           :in (clojure.lang.PersistentQueue/EMPTY)
                           :out (clojure.lang.PersistentQueue/EMPTY)
                           :sends 0
                           :blocked false}))

(defn apply-instruction-1 [instruction state]
  (let [{:keys [op arg-1 arg-2]} instruction
        cursor (:cursor state)
        state (update state :cursor inc)
        get-val (partial get-val state)]
    (trace instruction)
    (case op
      :snd (assoc state :snd (get-val arg-1))
      :rcv (if (pos? (get-val arg-1))
             (assoc state :recovered (:snd state))
             state)
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
    (loop [state (mk-init-state instructions)
           counter 0]
      (let [recovered (:recovered state)
            cursor (:cursor state)]
        (trace state)
        (if recovered
          recovered
          (recur (apply-instruction-1
                  (get instructions cursor)
                  state) (inc counter)))))))

(defn apply-instruction-2 [instruction state]
  (let [{:keys [op arg-1 arg-2]} instruction
        cursor (:cursor state)
        state (update state :cursor inc)
        get-val (partial get-val state)]
    (trace instruction)
    (case op
      :snd (-> state
               (update :out #(conj % (get-val arg-1)))
               (update :sends inc))
      :rcv (if (not-empty (:in state))
             (-> state
                 (set-val arg-1 (peek (:in state)))
                 (update :in pop)
                 (assoc :blocked false))
             (assoc state
                    :blocked true
                    :cursor cursor))
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

(defn solve-2 [steps]
  (let [instructions (parse-input steps)]
    (loop [state-0 (mk-init-state instructions 0)
           state-1 (mk-init-state instructions 1)
           counter 0]
      (let [state-0-in (:out state-1)
            state-1-in (:out state-0)
            state-0 (-> state-0
                        (update :in #(apply conj % state-0-in))
                        (assoc :out clojure.lang.PersistentQueue/EMPTY))
            state-1 (-> state-1
                        (update :in #(apply conj % state-1-in))
                        (assoc :out clojure.lang.PersistentQueue/EMPTY))
            ]
        (trace state-0)
        (trace state-1)
        (if (and
             (:blocked state-0)
             (:blocked state-1))
          (:sends state-1)
          (recur (apply-instruction-2
                  (get instructions (:cursor state-0))
                  state-0)
                 (apply-instruction-2
                  (get instructions (:cursor state-1))
                  state-1)
                 (inc counter)))))))

(def input (load-input-file "18"))

(defn solve-day-18 []
  (clojure.pprint/pprint (solve-1 input))
  (clojure.pprint/pprint (solve-2 input)))
