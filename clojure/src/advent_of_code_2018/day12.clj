(ns advent-of-code-2018.day12
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(def input (str/split (core/read-input "day12.txt") #"\n"))

(def initial-pots (vec (drop (count "initial state: ") (first input))))

(defn parse-rule [line]
  (let [[_ pattern result] (re-matches #"([.#]{5}) => ([.#])" line)]
    [(vec pattern) (first result)]))

(def rules (->> input
                (drop 2)
                (map parse-rule)
                (into {})))

(def padding [\. \. \. \.])

(defn trim-end [pots]
  (loop [pots pots]
    (if (= \. (last pots))
      (recur (butlast pots))
      pots)))

(defn trim-pots [pots]
  (let [[blank-pots rest-pots] (split-with #(= \. %) pots)
        trimmed-pots (trim-end rest-pots)]
    [trimmed-pots (count blank-pots)]))

(defn gen-plants [[pots ^int min-index]]
  (let [padded-pots (concat padding pots padding)
        new-min-index (- min-index 2)
        patterns (partition 5 1 padded-pots)
        new-pots (mapv rules patterns)
        [trimmed-pots ^int num-trimmed-front] (trim-pots new-pots)]
    [trimmed-pots (+ new-min-index num-trimmed-front)]))

(defn run-pot-gens [initial-pots target-gen]
  (loop [pots-and-index [initial-pots 0]
         curr-gen 0]
    (let [new-pots-and-index (gen-plants pots-and-index)
          new-gen (inc curr-gen)]
      (if (= new-gen target-gen)
        new-pots-and-index
        (recur new-pots-and-index new-gen)))))

(defn sum-filled-pots [pots min-index]
  (loop [pots pots
         pot-num min-index
         sum 0]
    (if (empty? pots)
      sum
      (let [[pot & rest-pots] pots]
        (recur rest-pots
               (inc pot-num)
               (+ sum (if (= \# pot) pot-num 0)))))))

(def gen-20 (run-pot-gens initial-pots 20))
(def gen-10000 (run-pot-gens initial-pots 10000))
(def gen-50000000000 [(first gen-10000) (+ 50000000000 -10000 (second gen-10000))])

; Part 1 Solution
(apply sum-filled-pots gen-20)

; Part 2 Solution
(apply sum-filled-pots gen-50000000000)

; TODO: Generalize - halt generation when pattern reaches fixed point;
;       use difference in min-indices to find offset per generation
