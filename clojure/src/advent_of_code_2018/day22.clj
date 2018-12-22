(ns advent-of-code-2018.day22
  (:require [advent-of-code-2018.core :as core]
            [clojure.set :as set]))

(set! *unchecked-math* :warn-on-boxed)

(def depth 5616)
(def target [10 785])

(defn build-geologic-index [target erosion-levels [x y :as region]]
  (cond
    (= [0 0] region) 0
    (= target region) 0
    (zero? ^int y)  (* ^int x 16807)
    (zero? ^int x)  (* ^int y 48271)
    :else (* ^int (erosion-levels [(dec ^int x) y])
             ^int (erosion-levels [x (dec ^int y)]))))

(defn build-erosion-level [target erosion-levels region]
  (if (erosion-levels region)
    erosion-levels
    (-> (build-geologic-index target erosion-levels region)
      (#(+ ^int depth ^int %))
      (mod 20183)
      (->> (assoc erosion-levels region)))))

(defn build-minimum-erosion-levels [target max-x max-y]
   (reduce #(build-erosion-level target %1 %2)
           {}
           (for [x (range (inc ^int max-x))
                 y (range (inc ^int max-y))]
             [x y])))

(defn erosion-level->risk-level [erosion-level]
  (mod erosion-level 3))

(defn erosion-levels->risk-levels [erosion-levels]
  (core/map-vals erosion-level->risk-level erosion-levels))

;; Part 1 Solution
(->> target
     (apply build-minimum-erosion-levels target)
     erosion-levels->risk-levels
     (#(assoc % target 0))
     vals
     (reduce +))

;; Part 2

(defn erosion-level->region-type [erosion-level]
  (case (mod erosion-level 3)
    0 :rocky
    1 :wet
    2 :narrow))

(defn erosion-levels->region-types [erosion-levels]
  (core/map-vals erosion-level->region-type erosion-levels))

(def all-tools #{:climbing-gear :torch :neither})

(def region-type->tools
  {:rocky  [:climbing-gear :torch]
   :wet    [:climbing-gear :neither]
   :narrow [:torch :neither]})

(defn erosion-levels->tools [erosion-levels]
  (core/map-vals (comp region-type->tools erosion-level->region-type) erosion-levels))

(defn neighboring-edges [[x y tool :as vertex] vertices]
  (let [[tool-a tool-b] (vec (disj all-tools tool))]
    (filter #(contains? vertices (second %))
            [[vertex [x y tool-a] 7]
             [vertex [x y tool-b] 7]
             [vertex [(inc x) y tool] 1]
             [vertex [(dec x) y tool] 1]
             [vertex [x (inc y) tool] 1]
             [vertex [x (dec y) tool] 1]])))

(defn build-cavern-jgraph [scale]
  (let [cavern-region-tools (->> target
                                 (mapv * scale)
                                 (apply build-minimum-erosion-levels target)
                                 erosion-levels->tools)
        cavern-graph (org.jgrapht.graph.SimpleDirectedWeightedGraph. org.jgrapht.graph.DefaultWeightedEdge)
        vertices (set (mapcat (fn [[coord [tool-a tool-b]]]
                           [(conj coord tool-a) (conj coord tool-b)])
                              cavern-region-tools))
        edges (mapcat #(neighboring-edges % vertices) vertices)]
    (doall (map #(.addVertex cavern-graph %) vertices))
    (doall (map (fn [[from-v to-v weight]]
                  (let [edge (.addEdge cavern-graph from-v to-v)]
                    (.setEdgeWeight cavern-graph edge weight)))
                edges))
    cavern-graph))

;; Part 2 Solution
(-> [5 5]
    build-cavern-jgraph
    org.jgrapht.alg.shortestpath.DijkstraShortestPath.
    (.getPathWeight [0 0 :torch] (conj target :torch)))
