(ns advent-of-code-2018.day25
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(def input (str/split-lines (core/read-input "day25.txt")))

(defn parse-fixed-point [line]
  (map core/parse-int (str/split line #",")))

(defn parse-fixed-points [input]
  (mapv parse-fixed-point input))

(defn distance [point-a point-b]
  (->> point-a
       (map #(Math/abs (- %1 %2)) point-b)
       (reduce +)))

(defn chainable? [point-a point-b]
  (<= (distance point-a point-b) 3))

(defn build-graph [input]
  (let [fixed-points (parse-fixed-points input)
        graph (org.jgrapht.graph.Pseudograph. org.jgrapht.graph.DefaultEdge)]
    (doall (map #(.addVertex graph %) fixed-points))
    (doall (for [i (range (dec (count fixed-points)))
                 j (range (inc i) (count fixed-points))
                 :let [edge-i (get fixed-points i)
                       edge-j (get fixed-points j)]]
             (when (chainable? edge-i edge-j)
               (.addEdge graph edge-i edge-j))))
    graph))

(def graph (build-graph input))

(defn get-graph-constellations [graph]
  (-> graph
      org.jgrapht.alg.ConnectivityInspector.
      .connectedSets
      .size))

;; Part 1 Solution
(-> input
    build-graph
    get-graph-constellations)
