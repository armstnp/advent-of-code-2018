(ns advent-of-code-2018.day24
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(def input (str/split-lines (core/read-input "day24.txt")))

;; Parsing

(defn parse-resistance [re s]
  (set
   (some->> s
            (re-find re)
            second
            (#(str/split % #", "))
            (map keyword))))

(core/defn-split parse-group [army | id line]
  (let [[_ count-str hp-str resistances attack-damage-str attack-type-str initiative-str]
        (re-matches #"(\d+) units each with (\d+) hit points (.+)?with an attack that does (\d+) (.+) damage at initiative (\d+)" line)
        id (str army "-" id)
        army (keyword army)
        [unit-count hp attack-damage initiative] (map core/parse-int [count-str hp-str attack-damage-str initiative-str])
        attack-type (keyword attack-type-str)
        weaknesses (parse-resistance #"weak to (.+?)(?:;|\))" resistances)
        immunities (parse-resistance #"immune to (.+?)(?:;|\))" resistances)]
    {:id id
     :army army
     :unit-count unit-count
     :hp    hp
     :damage attack-damage
     :attack-type attack-type
     :weaknesses weaknesses
     :immunities immunities
     :initiative initiative}))

(defn parse-armies [input]
  (let [[[_ & immune-lines] [_ _ & infection-lines]] (split-with #(not (empty? %)) input)
        groups (concat (map-indexed (parse-group "immune") immune-lines)
                       (map-indexed (parse-group "infected") infection-lines))]
    (->> groups
         (map #(vector (:id %) %))
         (into {}))))

;; Groups

(defn enemies? [{army-a :army} {army-b :army}]
  (not= army-a army-b))

(defn weak? [{:keys [weaknesses]} attack-type]
  (contains? weaknesses attack-type))

(defn immune? [{:keys [immunities]} attack-type]
  (contains? immunities attack-type))

(defn effective-power [{:keys [unit-count damage]}]
  (* ^int unit-count ^int damage))

(defn outgoing-damage [{:keys [attack-type] :as attacker} defender]
  (cond
    (immune? defender attack-type) 0
    (weak? defender attack-type) (* 2 ^int (effective-power attacker))
    :else (effective-power attacker)))

;; Target Selection

(defn sort-targeting [groups]
  (sort-by (fn [{:keys [initiative] :as group}]
             [(- ^int (effective-power group))
              (- ^int initiative)])
           groups))

(defn select-target [attacker targets]
  (->> targets
       (filter #(and (enemies? attacker %)
                     (> ^int (outgoing-damage attacker %) 0)))
       (sort-by #(vector (- ^int (outgoing-damage attacker %))
                         (- ^int (effective-power %))
                         (- ^int (:initiative %))))
       first))

(defn bind-target [{:keys [defenders bindings] :as selection-state} attacker]
  (if-let [target (select-target attacker defenders)]
    {:defenders (disj defenders target)
     :bindings (conj bindings {:attacker-id (:id attacker) :defender-id (:id target)})}
    selection-state))

(defn select-targets [groups]
  (let [attackers (sort-targeting groups)
        defenders (set groups)]
    (->> attackers
         (reduce bind-target {:defenders defenders :bindings []})
         :bindings)))

;; Attacking

(defn attack [attacker {:keys [unit-count hp] :as defender}]
  (let [damage (outgoing-damage attacker defender)
        unit-kills (quot ^int damage ^int hp)
        new-unit-count (max 0 (- ^int unit-count unit-kills))
        dead? (zero? new-unit-count)]
    (assoc defender
           :unit-count new-unit-count
           :dead? dead?)))

(defn attack-and-update [groups {:keys [attacker-id defender-id]}]
  (let [attacker (groups attacker-id)]
    (update groups defender-id #(attack attacker %))))

(defn run-attack-phase [groups target-bindings]
  (->> target-bindings
       (sort-by #(->> % :attacker-id groups :initiative -))
       (reduce attack-and-update groups)))

;; Battle

(defn battle-over? [groups]
  (->> groups
       vals
       (group-by :army)
       count
       (= 1)))

(defn fight [groups]
  (->> groups
       vals
       select-targets
       (run-attack-phase groups)
       (filter #(not (:dead? (second %))))
       (into {})))

(defn hp-remaining [groups]
  (->> groups
       vals
       (map :unit-count)
       (apply +)))

;; Part 1 Solution

(->> input
     parse-armies
     (iterate fight)
     (filter battle-over?)
     first
     hp-remaining)

;; Part 2

(defn winner [groups]
  (when (battle-over? groups)
    (-> groups
        vals
        first
        :army)))

(defn boost-immune [boost groups]
  (reduce-kv (fn [m k v]
               (assoc m k
                      (if (= :immune (:army v))
                        (update v :damage #(+ ^int % ^int boost))
                        v)))
             {}
             groups))

(defn boosted-fight-outcome [boost groups]
  (let [battle-end (->> groups
                        (boost-immune boost)
                        (iterate fight)
                        (partition 2 1)
                        (filter #(or (winner (second %)) (apply = %)))
                        first)]
    (if (apply = battle-end)
      [:stalemate nil]
      ((juxt winner hp-remaining) (second battle-end)))))

(defn seek-winning-outcome
  ([groups]
   (seek-winning-outcome groups 1))

  ([groups lower-bound]
   (let [boost (* 2 lower-bound)
         [winner hp-remaining] (boosted-fight-outcome boost groups)]
     (if (= winner :immune)
       (seek-winning-outcome groups lower-bound boost)
       (recur groups boost))))

  ([groups lower-bound upper-bound]
   (if (= (inc lower-bound) upper-bound)
     (second (boosted-fight-outcome upper-bound groups))
     (let [boost (+ lower-bound (quot (- upper-bound lower-bound) 2))
           [winner hp-remaining] (boosted-fight-outcome boost groups)]
       (if (= winner :immune)
         (recur groups lower-bound boost)
         (recur groups boost upper-bound))))))

;; Part 2 Solution
(seek-winning-outcome (parse-armies input))

;; Brute force doesn't take that much longer.
#_(let [groups (parse-armies input)]
  (->> (range)
       (map #(boosted-fight-outcome % groups))
       (filter #(= (first %) :immune))
       first
       second))
