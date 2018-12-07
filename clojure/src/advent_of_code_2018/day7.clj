(ns advent-of-code-2018.day7
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]
            [clojure.set :as set]))

(set! *unchecked-math* :warn-on-boxed)

(def input (core/read-input "day7.txt"))

(def step-re #"Step (.) must be finished before step (.) can begin\.")

(defn parse-step [line]
  (map (comp int first) (rest (re-matches step-re line))))

(def dependencies (map parse-step (str/split input #"\n")))

; Part 1
(defn vertices [graph]
  (set (mapcat identity graph)))

(def graph (org.jgrapht.graph.DirectedAcyclicGraph. org.jgrapht.graph.DefaultEdge))
(doall (map #(.addVertex graph %) (vertices dependencies)))
(doall (map (fn [[src dest]] (.addEdge graph src dest)) dependencies))
(def graph-iterator (org.jgrapht.traverse.TopologicalOrderIterator. graph <))
(def graph-walk
  (loop [steps []]
    (if (.hasNext graph-iterator)
      (recur (conj steps (.next graph-iterator)))
      steps)))

; Part 1 Solution
(apply str (map char graph-walk))

; Part 2

(defn time-to-complete [step]
  (- step 4))

(defn independent [graph exclusions dropped]
  (sort (set/difference (set/union (vertices graph) dropped) (set (map second graph)) exclusions)))

(defn remove-vertex [graph vertex]
  (filter #(not= vertex (first %)) graph))

(def max-workers 5)
(defn ->worker [step]
  [(time-to-complete step) step])
(def worker->time-left first)
(def worker->vertex second)

(defn tick-graph-with-worker [graph [time-left vertex :as worker]]
  (let [new-time-left (dec time-left)]
    (if (zero? new-time-left)
      (remove-vertex graph vertex)
      graph)))

(defn tick-worker [[time-left vertex :as worker]]
  (let [new-time-left (dec time-left)]
    (when (not (zero? new-time-left)) [new-time-left vertex])))

(defn dropped-vertices [old-graph new-graph]
  (let [diff-edges (set/difference (set old-graph) (set new-graph))
        possible-leaves (set (map second diff-edges))
        present-vertices (vertices new-graph)]
    (set/difference possible-leaves present-vertices)))

(defn tick-workers [[graph workers]]
  (let [new-graph       (reduce tick-graph-with-worker graph workers)
        dropped         (dropped-vertices graph new-graph)
        ticked-workers  (filter #(not (nil? %)) (map tick-worker workers))
        waiting-workers (- max-workers (count ticked-workers))
        wip             (set (map worker->vertex ticked-workers))
        available-work  (independent new-graph wip dropped)
        alloc-work      (take waiting-workers available-work)
        added-workers   (map ->worker alloc-work)
        new-workers     (into ticked-workers added-workers)]
    [new-graph new-workers]))

(->> [dependencies []]
     (iterate tick-workers)
     (take-while #(not (and (empty? (first %)) (empty? (second %)))))
     (drop 1)
     count)
