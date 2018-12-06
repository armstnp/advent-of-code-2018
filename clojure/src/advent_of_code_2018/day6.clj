(ns advent-of-code-2018.day6
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (core/read-input "day6.txt"))

(defn parse-point [line]
  (->> line
       (re-matches #"(\d+), (\d+)")
       rest
       (map core/parse-int)))

(def input-points (mapv parse-point (str/split input #"\n")))
(def id-points (map-indexed vector input-points))
(def ->index first)
(def ->point second)

(defn distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(def min-x (- (apply min (map first input-points)) 10))
(def max-x (+ (apply max (map first input-points)) 11))
(def min-y (- (apply min (map second input-points)) 10))
(def max-y (+ (apply max (map second input-points)) 11))
(def all-coords
  (for [x (range min-x max-x)
        y (range min-y max-y)]
    [x y]))
(def boundaries
  (set/union (map #(vector min-x %) (range min-y max-y))
             (map #(vector (dec max-x) %) (range min-y max-y))
             (map #(vector % min-y) (range min-x max-x))
             (map #(vector % (dec max-y)) (range min-x max-x))))

(core/defn-split paint-coord-closest-point [points | canvas [x y :as in-coord]]
  (let [[[closest-a-id closest-a-dist] [_ closest-b-dist]]
        (->> points
             (map #(vector (->index %) (distance in-coord (->point %))))
             (sort-by second))

        closest-id (when (not= closest-a-dist closest-b-dist) closest-a-id)]

    (assoc canvas in-coord closest-id)))

(def painted-canvas (reduce (paint-coord-closest-point id-points) {} all-coords))

(def infinite-ids (into (set (map painted-canvas boundaries)) nil))

(def id-count (apply dissoc
                     (reduce-kv #(update %1 %3 (fnil inc 0))
                                {}
                                painted-canvas)
                     infinite-ids))

(->> id-count
     (map second)
     (apply max))

; Part 2

(core/defn-split paint-coord-suitable-drop [points | canvas [x y :as in-coord]]
  (let [coord-val (if (->> points
                           (map #(distance in-coord %))
                           (reduce +)
                           (> 10000))
                    1 0)]
    (assoc canvas in-coord coord-val)))

(def hyper-painted-canvas (reduce (paint-coord-suitable-drop input-points) {} all-coords))

(->> hyper-painted-canvas
     (map second)
     (reduce +))
