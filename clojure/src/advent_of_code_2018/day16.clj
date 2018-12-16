(ns advent-of-code-2018.day16
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]
            [clojure.set :as set]))

(set! *unchecked-math* :warn-on-boxed)

(def input (str/split (core/read-input "day16.txt") #"\n"))

;; Instructions

#_(core/defn-split read-value [v-type address | registers]
  (case v-type
    :register (get registers address)
    :immediate address))

(core/defn-split immediate [value | registers]
  value)

(core/defn-split register [address | registers]
  (get registers address))

(core/defn-split ignore [ignored | registers]
  nil)

(core/defn-split reg-out [address | value registers]
  (assoc registers address value))

(defn parse-addr [a b c]
  {:a (register a)
   :b (register b)
   :c (reg-out c)
   :op +})

(defn parse-addi [a b c]
  {:a (register a)
   :b (immediate b)
   :c (reg-out c)
   :op +})

(defn parse-mulr [a b c]
  {:a (register a)
   :b (register b)
   :c (reg-out c)
   :op *})

(defn parse-muli [a b c]
  {:a (register a)
   :b (immediate b)
   :c (reg-out c)
   :op *})

(defn parse-banr [a b c]
  {:a (register a)
   :b (register b)
   :c (reg-out c)
   :op bit-and})

(defn parse-bani [a b c]
  {:a (register a)
   :b (immediate b)
   :c (reg-out c)
   :op bit-and})

(defn parse-borr [a b c]
  {:a (register a)
   :b (register b)
   :c (reg-out c)
   :op bit-or})

(defn parse-bori [a b c]
  {:a (register a)
   :b (immediate b)
   :c (reg-out c)
   :op bit-or})

(defn parse-setr [a b c]
  {:a (register a)
   :b (ignore b)
   :c (reg-out c)
   :op (fn [x y] x)})

(defn parse-seti [a b c]
  {:a (immediate a)
   :b (ignore b)
   :c (reg-out c)
   :op (fn [x y] x)})

(core/defn-split comparison [op | x y]
  (if (op x y) 1 0))

(def gt (comparison >))
(def eq (comparison =))

(defn parse-gtir [a b c]
  {:a (immediate a)
   :b (register b)
   :c (reg-out c)
   :op gt})

(defn parse-gtri [a b c]
  {:a (register a)
   :b (immediate b)
   :c (reg-out c)
   :op gt})

(defn parse-gtrr [a b c]
  {:a (register a)
   :b (register b)
   :c (reg-out c)
   :op gt})

(defn parse-eqir [a b c]
  {:a (immediate a)
   :b (register b)
   :c (reg-out c)
   :op eq})

(defn parse-eqri [a b c]
  {:a (register a)
   :b (immediate b)
   :c (reg-out c)
   :op eq})

(defn parse-eqrr [a b c]
  {:a (register a)
   :b (register b)
   :c (reg-out c)
   :op eq})

(defn exec-op [{:keys [a b c op]} registers]
  (let [a-val (a registers)
        b-val (b registers)
        c-val (op a-val b-val)]
    (c c-val registers)))

;;;;;;;;;;;;;
;; Deductions
;;;;;;;;;;;;;

(defn parse-example [[before-line op-line after-line]]
  (let [[op :as instruction] (map core/parse-int (str/split op-line #" "))]
    {:before (->> before-line
                  (re-matches #"Before: \[(\d), (\d), (\d), (\d)\]")
                  (drop 1)
                  (map core/parse-int))
     :op op
     :instruction instruction
     :after (->> after-line
                 (re-matches #"After:  \[(\d), (\d), (\d), (\d)\]")
                 (drop 1)
                 (map core/parse-int))}))

(def examples
  (->> input
       (partition 3 4)
       (take-while #(not (empty? (first %))))
       (map parse-example)))

(def op-code->parse-fn
  {:addr parse-addr
   :addi parse-addi
   :mulr parse-mulr
   :muli parse-muli
   :banr parse-banr
   :bani parse-bani
   :borr parse-borr
   :bori parse-bori
   :setr parse-setr
   :seti parse-seti
   :gtir parse-gtir
   :gtri parse-gtri
   :gtrr parse-gtrr
   :eqir parse-eqir
   :eqri parse-eqri
   :eqrr parse-eqrr})

(def op-codes (keys op-code->parse-fn))

(defn satisfies-example? [op-code {[op-n & instruction] :instruction :keys [before after]}]
  (-> op-code
      op-code->parse-fn
      (apply instruction)
      (exec-op (vec before))
      (= after)))

; Part 1 Solution
(->> (for [example examples]
       (filter #(satisfies-example? % example) op-codes))
     (filter #(>= (count %) 3))
     count)

(defn counter-examples [op-codes examples]
  (set
   (for [example examples
         op-code op-codes
         :when (not (satisfies-example? op-code example))]
     [(:op example) op-code])))

(defn possible-bindings [op-codes examples]
  (set/difference
   (set
    (for [example examples
          op-code op-codes]
      [(:op example) op-code]))
   (counter-examples op-codes examples)))

(defn seek-op-binding [op-codes examples]
  (->> (possible-bindings op-codes examples)
       (group-by first)
       (filter #(= (count (second %)) 1))
       first
       second
       first))

(defn bind-ops [op-codes examples]
  (loop [op-codes op-codes
         bound-ops {}]
    (if (empty? op-codes)
      bound-ops
      (let [[op-n op-code :as bound-op] (seek-op-binding op-codes examples)]
        (if (nil? bound-op)
          [:unmatched op-codes]
          (recur (disj op-codes op-code)
                 (assoc bound-ops op-n op-code)))))))

(def op-bindings
  (->> examples
       (bind-ops (set op-codes))
       (map #(vector (first %) (op-code->parse-fn (second %))))
       (into {})))

;; Program

(def init-registers [0 0 0 0])

(defn parse-instruction [line]
  (let [[op-n a b c] (map core/parse-int (str/split line #" "))
        op (op-bindings op-n)]
    (op a b c)))

(def program
  (->> input
       (partition-all 4)
       (drop-while #(str/starts-with? (first %) "Before"))
       (mapcat identity)
       (filter #(not (empty? %)))
       (map parse-instruction)))

;; Part 2 Solution
(reduce #(exec-op %2 %1) init-registers program)
