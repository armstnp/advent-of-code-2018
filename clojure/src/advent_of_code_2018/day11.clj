(ns advent-of-code-2018.day11
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(def serial 8199)

(def power-level
  (memoize
   (fn [x y]
     (let [rack-id (+ 10 ^int x)]
       (- (quot ^int (mod (* (+ (* rack-id ^int y)
                                ^int serial)
                             rack-id)
                          1000)
                100)
          5)))))

(def grid
  (into {}
        (for [x (range 1 301)
              y (range 1 301)]
          [[x y] (power-level x y)])))

(def chunked-grid
  (into {}
        (for [x3 (partition 3 1 (range 1 301))
              y3 (partition 3 1 (range 1 301))]
          (vector [(first x3) (first y3)]
                  (apply + (for [x x3
                                 y y3]
                             (grid [x y])))))))


; Part 1 Solution
(apply max-key second chunked-grid)

(def init-meta-grid
  (into {} (map (fn [[[x y] power]]
                  [[x y 1] power])
                grid)))

; greatest whole divisor
(defn gwd [n]
  (quot ^int n ^int (first (drop-while #(> ^int (mod ^int n ^int %) 0) (range 2 (inc ^int n))))))

(defn sub-squares [x y dim grid]
  (let [half (quot ^int dim 2)]
    (if (even? dim)
      (reduce + (map grid
                     (for [dx [0 half]
                           dy [0 half]]
                       [(+ ^int x ^int dx) (+ ^int y ^int dy) half])))
      (let [greater-half (inc half)]
        (- ^int (reduce + (map grid [[x                       y                       greater-half]
                                     [(+ ^int x greater-half) y                       half]
                                     [x                       (+ ^int y greater-half) half]
                                     [(+ ^int x half)         (+ ^int y half)         greater-half]]))
           ^int (grid [(+ ^int x half) (+ ^int y half) 1]))))))

(defn build-grid-chunks [dim grid]
  (println (str "Building grid for dimension " dim))
  (into {}
        (for [ul-x (range 1 (- 301 dim))
              ul-y (range 1 (- 301 dim))]
          (vector [ul-x ul-y dim]
                  (sub-squares ul-x ul-y dim grid)))))

#_(defn build-grid-chunks [dim grid]
  (println (str "Building grid for dimension " dim))
  (into {}
        (let [sub-chunk-size (gwd dim)]
          (for [ul-x (range 1 (- 301 dim))
                ul-y (range 1 (- 301 dim))]
            (vector [ul-x ul-y dim]
                    (apply + (for [x (range ul-x (+ ^int ul-x ^int dim) sub-chunk-size)
                                   y (range ul-y (+ ^int ul-y ^int dim) sub-chunk-size)]
                               (grid [x y sub-chunk-size]))))))))

(def chunked-grid-meta
  (reduce #(assoc %1 %2 (build-grid-chunks %2 %1)) init-meta-grid (range 2 301)))

#_chunked-grid-meta 
