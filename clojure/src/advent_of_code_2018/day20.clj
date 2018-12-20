(ns advent-of-code-2018.day20
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(def input (-> "day20.txt"
               core/read-input
               str/trim-newline
               vec
               next
               butlast))

(def direction? #{\N \E \S \W})
(def open-alts? #{\(})
(def close-alts? #{\)})
(def sep-alts? #{\|})

(defn move-in-dir [[x y] dir]
  (case dir
    \N [x (dec ^int y)]
    \E [(inc ^int x) y]
    \S [x (inc ^int y)]
    \W [(dec ^int x) y]))

(def opposite-dir
  {\N \S
   \E \W
   \S \N
   \W \E})

(core/defn-split add-exit [exit | facility position]
  (update facility position (fn [exits] ((fnil conj #{}) exits exit))))

(defn move-superset [{:keys [positions facility] :as state} exit]
  (let [new-positions (set (map #(move-in-dir % exit) positions))
        facility-with-exits (reduce (add-exit exit) facility positions)
        entrance (opposite-dir exit)
        facility-with-entrances (reduce (add-exit entrance)
                                        facility-with-exits
                                        new-positions)]
    (assoc state
           :positions new-positions
           :facility facility-with-entrances)))

(defn push-cont [{:keys [positions] :as state}]
  (update state :cont-stack
          #(conj % {:frozen positions :accum #{}})))

(defn push-alt [{[cont & cont-rest] :cont-stack
                 :keys [positions]
                 :as state}]
  (assoc state
         :positions (:frozen cont)
         :cont-stack (conj cont-rest
                           (update cont :accum #(into % positions)))))

(defn pop-cont [{[cont & cont-rest] :cont-stack
                 :keys [positions]
                 :as state}]
  (assoc state
         :positions (into positions (:accum cont))
         :cont-stack cont-rest))

(defn move [state c]
  (cond-> state
    (direction? c)  (move-superset c)
    (open-alts? c)  push-cont
    (sep-alts? c)   push-alt
    (close-alts? c) pop-cont))

(def initial-state
  {:positions #{[0 0]}
   :cont-stack '()
   :facility {[0 0] #{}}})

(def facility (:facility (reduce move initial-state input)))

(defn traverse [facility]
  (loop [queue [[0 0]]
         distances {[0 0] 0}]
    (if (empty? queue)
      distances
      (let [position (peek queue)
            neighbors (map #(move-in-dir position %) (facility position))
            new-neighbors (filter #(not (distances %)) neighbors)
            neighbor-distance (inc ^int (distances position))
            new-distances (into distances
                                (map #(vector % neighbor-distance)
                                     new-neighbors))
            new-queue (into (pop queue) new-neighbors)]
        (recur new-queue new-distances)))))

;; Part 1 Solution
(->> facility
     traverse
     (sort-by (comp - second))
     first)

;; Part 2 Solution
(->> facility
     traverse
     (filter #(>= ^int (second %) 1000))
     count)
