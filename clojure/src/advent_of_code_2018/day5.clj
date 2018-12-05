(ns advent-of-code-2018.day5
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(def input (str/trim (core/read-input "day5.txt")))

(defn collide? [unit-a unit-b]
  (and (= (Character/toUpperCase unit-a) (Character/toUpperCase unit-b))
       (or (and (Character/isLowerCase unit-a) (Character/isUpperCase unit-b))
           (and (Character/isUpperCase unit-a) (Character/isLowerCase unit-b)))))

(defn push-unit [stack unit]
  (cond
    (empty? stack)
    (list unit)

    (collide? (first stack) unit)
    (rest stack)

    :else
    (conj stack unit)))

(defn collapsed-size [polymer]
  (count (reduce push-unit '() polymer)))

; Part 1 solution
(collapsed-size input)

(defn not-unit-type? [t]
  (let [t-type (Character/toLowerCase t)]
    (fn [unit] (not= t-type (Character/toLowerCase unit)))))

(def all-units (map char (range (int \a) (inc (int \z)))))

(defn filter-polymer [polymer unit-type]
  (let [type-filter (not-unit-type? unit-type)]
    (filter type-filter polymer)))

; Part 2 solution
(->> all-units
     (pmap #(vector % (collapsed-size (filter-polymer input %))))
     (sort-by second)
     first
     second)
