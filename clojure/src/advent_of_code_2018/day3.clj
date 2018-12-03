(ns advent-of-code-2018.day3
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (core/read-input "day3.txt"))
(def input-lines (str/split input #"\n"))

(def line-pattern #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
(defn parse-input [line]
  (let [[_ & groups] (re-matches line-pattern line)
        [id left top width height] (map core/parse-int groups)]
    {:id id :left left :top top :width width :height height}))

(def parsed-input
  (->> input-lines
       (map parse-input)))

(defn left [rect] (:left rect))
(defn top [rect] (:top rect))
(defn right [rect] (+ (left rect) (:width rect)))
(defn bottom [rect] (+ (top rect) (:height rect)))
(defn is-in-rect? [rect x y]
  (and (>= x (left rect))
       (< x (right rect))
       (>= y (top rect))
       (< y (bottom rect))))

(defn overlapping-squares [rect-a rect-b]
  (set
   (for [x (range (left rect-a) (right rect-a))
         y (range (top rect-a) (bottom rect-a))
         :when (is-in-rect? rect-b x y)]
     [x y])))

(defn overlap-all [[curr-rect & other-rects] overlaps]
  (if (empty? other-rects)
    overlaps
    (->> other-rects
         (map #(overlapping-squares curr-rect %))
         (reduce into overlaps)
         (recur other-rects))))

;; Part 2
#_(count (overlap-all parsed-input #{}))

(defn non-overlapping [rect-set]
  (let [curr-rect (first rect-set)
        remainder (disj rect-set curr-rect)
        collided (filter #(not (empty? (overlapping-squares curr-rect %))) remainder)
        tagged-collided (set (map #(assoc % :collided true) collided))]
    (if (and (empty? collided) (not (:collided curr-rect)))
      curr-rect
      (recur (into (set/difference remainder (set collided)) tagged-collided)))))

;; Solution 2
#_(:id (non-overlapping (set parsed-input)))
