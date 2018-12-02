(ns advent-of-code-2018.day2
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(def input (core/read-input "day2.txt"))

(def parsed-input (str/split input #"\n"))

;; Part 1 - Checksum

(defn words-with-n-same [words n]
  (->> words
       (map (comp set distinct vals frequencies))
       (filter #(% n))
       count))

(def words-with-2-same (words-with-n-same parsed-input 2))
(def words-with-3-same (words-with-n-same parsed-input 3))
(def checksum (* words-with-2-same words-with-3-same))

;; Part 2 - Distance

(defn distance [s1 s2]
  (reduce +
          (map (fn [a b] (if (= a b) 0 1))
               s1 s2)))

(defn single-distance [words]
  (first
   (for [x words
         y words
         :when (= 1 (distance x y))]
     [x y])))

(defn same-chars [s1 s2]
  (->> (map vector s1 s2)
       (filter #(apply = %))
       (map first)
       (apply str)))

(def matching-box-ids (apply same-chars (single-distance parsed-input)))

;; Solutions

checksum
matching-box-ids
