(ns advent-of-code-2018.day9
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(def num-players 448)
(def max-marble 71628)

(def all-marbles (range 1 (inc max-marble)))
(def player-cycle (cycle (range 1 (inc num-players))))
(def player-marbles (map vector player-cycle all-marbles))

(defn is-scoring-marble? [^long marble]
  (and (not (zero? marble)) (= 0 (mod marble 23))))

(core/rotate-right 1 [1 2 3 4])
#_(4 1 2 3)

(core/rotate-left 1 [1 2 3 4])
#_(2 3 4 1)

(core/defn-split default [d | x]
  (if (nil? x) d x))

(def default-zero (default 0))

(defn score-marble [{:keys [^java.util.LinkedList circle ^int curr-index scores] :as state} player ^long marble]
  (let [ccw-rotated (mod (- curr-index 7) (.size circle))
        removed-marble (.get circle ccw-rotated)
        _ (.remove circle ^int ccw-rotated)
        updated-scores (update scores player #(+ ^long (default-zero %) marble ^long removed-marble))
        updated-index (mod ccw-rotated (.size circle))]
    (assoc state
           :scores updated-scores
           :curr-index updated-index)))

(defn place-marble [{:keys [circle ^int curr-index scores] :as state} player marble]
  (if (is-scoring-marble? marble)
    (score-marble state player marble)
    (let [new-index (mod (+ 2 curr-index) (.size circle))]
        (.add circle new-index marble)
        (assoc state :curr-index new-index))))

(defn build-initial-list []
  (doto (new java.util.LinkedList) (.add 0)))

(def final-result
  (reduce (fn [state [player marble]] (place-marble state player marble))
          {:circle (build-initial-list) :curr-index 0 :scores {}}
          player-marbles))

#_(->> final-result
       :scores
       (map second)
       (apply max))

; Part 2

(def all-marbles-times-100 (range 1 (inc (* 100 max-marble))))
(def player-marbles-times-100 (map vector player-cycle all-marbles-times-100))

(def final-result-times-100
  (reduce (fn [state [player marble]] (place-marble state player marble))
          {:circle (build-initial-list) :curr-index 0 :scores {}}
          player-marbles-times-100)) 

(defn top-100-score []
  (->> final-result-times-100
       :scores
       (map second)
       (apply max)))

(defn -main [& args]
  (println (str "Top x100 score: " (top-100-score))))

(comment Not functional as of end of day. Moved over to Java.)
