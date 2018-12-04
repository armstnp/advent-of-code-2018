(ns advent-of-code-2018.day3b
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Input Handling

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

(defn id [rect] (:id rect))
(defn left [rect] (:left rect))
(defn top [rect] (:top rect))
(defn right [rect] (+ (left rect) (:width rect)))
(defn bottom [rect] (+ (top rect) (:height rect)))

;; Part 1

(defn mark-square [canvas square id]
  (update-in
   canvas
   square
   (fn [curr-val] (if (nil? curr-val) id :collided))))

(defn all-squares [rect]
  (for [x (range (left rect) (right rect))
        y (range (top rect) (bottom rect))]
    [x y]))

(defn mark-squares [canvas rect]
  (reduce
   #(mark-square %1 %2 (id rect))
   canvas
   (all-squares rect)))

(def filled-canvas (reduce mark-squares {} parsed-input))

;; Part 1 Solution
(->> filled-canvas
     vals
     (mapcat vals)
     (filter #(= % :collided))
     count)

;; Part 2

(defn collides? [canvas rect]
  (let [id (id rect)]
    (some #(not= id (get-in canvas %)) (all-squares rect))))

;; Part 2 Solution
(first (drop-while #(collides? filled-canvas %) parsed-input))
