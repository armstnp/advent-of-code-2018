(ns advent-of-code-2018.day1
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(def input (core/read-input "day1.txt"))

(def parsed-input (map core/parse-int (str/split input #"\n")))

;; Part one answer
(reduce + parsed-input) 

;; Part two answer

(defn seek-duplicate
  [seen-vals new-val]
  (if (seen-vals new-val)
    (reduced new-val)
    (conj seen-vals new-val)))

(reduce seek-duplicate #{0} (reductions + (cycle parsed-input))) 
