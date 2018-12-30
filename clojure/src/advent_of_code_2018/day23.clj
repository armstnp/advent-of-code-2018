(ns advent-of-code-2018.day23
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]
            [clojure.set :as set])
  (:import
   (java.util PriorityQueue)))

(set! *unchecked-math* :warn-on-boxed)

(def input (str/split-lines (core/read-input "day23.txt")))

(defn parse-nanobot [line]
  (let [[_ & matches] (re-matches #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" line)
        [x y z radius] (map core/parse-int matches)]
    {:pos [x y z]
     :radius radius}))

(defn distance ^long [pos-a pos-b]
  (->> pos-a
       (map - pos-b)
       (map #(Math/abs ^long %))
       (apply +)))

(defn in-range? [{pos-a :pos :keys [radius]} pos-b]
  (< ^long (distance pos-a pos-b) ^long radius))

;; Part 1 Solution
(let [nanobots (map parse-nanobot input)
      largest-nanobot (apply max-key :radius nanobots)]
  (->> nanobots
       (filter #(in-range? largest-nanobot (:pos %)))
       count))

;; Credit to ypsu for outlining an algorithm that is sufficient for today's problem!
;; https://www.reddit.com/r/adventofcode/comments/a99n7x/2018_day_23_part_2_explanation_with_visualization/

(defn nanobot-bounds [{:keys [pos radius]}]
  (map #(vector (- ^long % ^long radius) (+ ^long % ^long radius)) pos))

(defn nanobots-bounds [nanobots]
  (->> nanobots
       (map nanobot-bounds)
       core/transpose
       (map (juxt (comp #(apply min %) first) (comp #(apply max %) second)))))

(defn log2 ^double [^double x]
  (/ (Math/log x) (Math/log 2)))

(defn bounding-cube-length [bounds]
  (->> bounds
       (map (fn [[min-x max-x]] (- ^long max-x ^long min-x)))
       (apply max)
       double
       log2
       Math/ceil
       (Math/pow 2)
       Math/round))

(defn intersects-cube? [{:keys [root length]} {:keys [pos radius]}]
  (let [bounds (map #(vector % (+ ^long % (dec ^long length))) root)]
    (->> bounds
         (map (fn [pos-x [bound-min bound-max]]
                (cond
                  (< ^long pos-x ^long bound-min) (- ^long bound-min ^long pos-x)
                  (> ^long pos-x ^long bound-max) (- ^long pos-x ^long bound-max)
                  :else 0))
              pos)
         (reduce +)
         (#(>= ^long radius ^long %)))))

(defn prioritize [{:keys [root length intersections] :as cube}]
  (assoc cube :priority [(- ^int intersections)
                         (distance-to-origin cube)
                         length]))

(defn ->cube [root length nanobots]
  (let [cube {:root root :length length}]
    (->> nanobots
         (filter #(intersects-cube? cube %))
         count
         (assoc cube :intersections)
         prioritize)))

(defn bounding-cube [nanobots]
  (let [num-nanobots (count nanobots)]
    (loop [length (bounding-cube-length (nanobots-bounds nanobots))]
      (let [root (->> 2
                      (/ ^long length)
                      (#(- ^long %))
                      (repeat 3)
                      vec)
            cube (->cube root length nanobots)]
        (if (= num-nanobots (:intersections cube))
          cube
          (recur (* 2 ^long length)))))))

(defn corners [[x y z] length]
  (for [nx [x (+ ^long length ^long x)]
        ny [y (+ ^long length ^long y)]
        nz [z (+ ^long length ^long z)]]
    [nx ny nz]))

(defn distance-to-origin [{:keys [root length]}]
  (->> (corners root (dec ^long length))
       (map #(distance [0 0 0] %))
       (apply min)))

(defn subdivide [nanobots {:keys [root length]}]
  (let [half-length (/ ^long length 2)]
    (map #(->cube % half-length nanobots) (corners root half-length))))

(defn subdivision-search [nanobots cube]
  (let [queue (doto (PriorityQueue. 8 #(compare (:priority %1) (:priority %2)))
                (.offer cube))]
    (loop [{:keys [length] :as cube} (.poll queue)]
      (if (= 1 (:length cube))
        (:root cube)
        (do
          (doall (map #(.offer queue %) (subdivide nanobots cube)))
          (recur (.poll queue)))))))

;; Part 2 solution
(let [nanobots (map parse-nanobot input)]
  (->> nanobots
       bounding-cube
       (subdivision-search nanobots)
       (distance [0 0 0])))
