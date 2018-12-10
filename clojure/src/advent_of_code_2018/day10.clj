(ns advent-of-code-2018.day10
   (:require [advent-of-code-2018.core :as core]
             [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(def input (core/read-input "day10.txt"))

(def line-re #"position=<\s*([-0-9]+),\s*([-0-9]+)> velocity=<\s*([-0-9]+),\s*([-0-9]+)>")

(defn parse-line [line]
  (let [[_ & str-data] (re-matches line-re line)
        [px py vx vy] (map core/parse-int str-data)]
    {:pos [px py]
     :vel [vx vy]}))

(def parsed-input (map parse-line (str/split input #"\n")))

(defn tick-point [{:keys [pos vel] :as point}]
  (assoc point :pos (mapv + pos vel)))

(defn bounds [points]
  (let [positions (map :pos points)]
    (let [min-x     (apply min (map first positions))
          min-y     (apply min (map second positions))
          max-x     (apply max (map first positions))
          max-y     (apply max (map second positions))]
      {:min-x  min-x
       :min-y  min-y
       :max-x  max-x
       :max-y  max-y
       :width  (- max-x min-x)
       :height (- max-y min-y)})))

(defn bounding-area [points]
  (let [{:keys [^int width ^int height]} (bounds points)]
    (* width height)))

(defn trim-to-bounds [points]
  (let [{:keys [min-x min-y]} (bounds points)]
    (map #(update % :pos (fn [pos] (mapv - pos [min-x min-y])))
         points)))

(defn map-star-chart [stars]
  (let [{:keys [width height]} (bounds stars)
        stars-set (set (map :pos stars))
        header (str "P3\n" width " " height "\n" "1\n")
        body (str/join (for [y (range 0 height)
                             x (range 0 width)]
                         (if (stars-set [x y])
                           "1 1 0\n"
                           "0 0 0\n")))
        filename "star-chart.ppm"]
    (spit filename header :append true)
    (spit filename body :append true)))

; Part 1 solution
(def aligned-stars
  (->> parsed-input
       (iterate #(map tick-point %))
       (partition 2 1)
       (drop-while #(> ^long (bounding-area (first %)) ^long (bounding-area (second %))))
       first
       first
       trim-to-bounds
       map-star-chart))

; Part 2 solution
(def time-taken
  (->> {:stars parsed-input :t 0}
       (iterate (fn [{:keys [stars t] :as state}]
                  (assoc state
                         :stars (map tick-point stars)
                         :t (inc t))))
       (partition 2 1)
       (drop-while #(> ^long (bounding-area (:stars (first %)))
                       ^long (bounding-area (:stars (second %)))))
       first
       first
       :t))

