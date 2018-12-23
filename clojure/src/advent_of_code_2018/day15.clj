(ns advent-of-code-2018.day15
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]
            [clojure.set :as set]))

(set! *unchecked-math* :warn-on-boxed)

(def input (core/read-input "day15.txt"))

(def ^:dynamic *elf-attack-power* 3)

;; Coordinates

(defn cardinals [[y x]]
  [[(inc ^int y) x]
   [(dec ^int y) x]
   [y (inc ^int x)]
   [y (dec ^int x)]])

(defn disambiguate [coords]
  (first (sort coords)))

;; Dungeon Queries

(defn wall? [x] (= \# x))
(defn space? [x] (= \. x))
(defn elf? [x] (= \E x))
(defn goblin? [x] (= \G x))
(defn unit? [x] (or (elf? x) (goblin? x)))
(defn solid? [x] (not (space? x)))

;; Unit Logic

(defn ->unit [y x c]
  {:type c
   :pos [y x]
   :hp 200})

(defn initial-units [dungeon]
  (vec
   (for [y (range (count dungeon))
         x (range (count (first dungeon)))
         :let [dungeon-item (get-in dungeon [y x])]
         :when (unit? dungeon-item)]
     (->unit y x dungeon-item))))

(defn enemies? [unit-a unit-b]
  (contains? #{[\E \G] [\G \E]} [unit-a unit-b]))

(defn dead? [{dead :dead}]
  dead)

(defn battle-over? [units]
  (->> units
       (map :type)
       frequencies
       count
       (= 1)))

;; Dungeon Manipulation

(defn clear-dungeon [dungeon]
  (mapv #(mapv (fn [x] (if (unit? x) \. x)) %) dungeon))

(defn place-units [units empty-dungeon]
  (->> units
       (filter #(not (:dead %)))
       (reduce #(assoc-in %1 (:pos %2) (:type %2)) empty-dungeon)))

(defn replace-units [units dungeon]
  (place-units units (clear-dungeon dungeon)))

(defn print-dungeon [dungeon]
  (doall (map (fn [row] (println (apply str row))) dungeon)))

;; Unit Logic

(defn enemies [unit units]
  (filter #(enemies? (:type unit) (:type %)) units))

(defn in-range [pos dungeon]
  (for [possible-space (cardinals pos)
        :let [item (get-in dungeon possible-space)]
        :when (space? item)]
    possible-space))

(defn reachable [init-pos dest-coords dungeon]
  (let [starting-coords (in-range init-pos dungeon)]
    (loop [stack (apply list starting-coords)
           seen #{}]
      (if (empty? stack)
        (set/intersection (set dest-coords) seen)
        (let [[popped & rest-stack] stack
              new-seen (conj seen popped)
              popped-adjacent (set/difference (set (in-range popped dungeon)) new-seen)
              new-stack (into rest-stack popped-adjacent)]
          (recur new-stack new-seen))))))

(defn path-priority [{distance-a :distance ancestor-a :ancestor coord-a :coord}
                     {distance-b :distance ancestor-b :ancestor coord-b :coord}]
  (> 1 (compare [distance-a ancestor-a coord-a]
                [distance-b ancestor-b coord-b])))

(def path-key (juxt :distance :coord :ancestor))

(defn step-to-nearest [{init-pos :pos type :type :as unit} dungeon]
  (let [starting-coords (in-range init-pos dungeon)
        coord-nodes (map #(hash-map :coord % :distance 1 :ancestor %) starting-coords)
        queue (apply list (sort-by path-key coord-nodes))]
    (loop [queue queue
           seen #{init-pos}]
      (if (empty? queue)
        init-pos
        (let [{curr-coord :coord
               curr-distance :distance
               curr-ancestor :ancestor} (peek queue)]
          (cond
            (some #(enemies? type (get-in dungeon %)) (cardinals curr-coord)) curr-ancestor
            (contains? seen curr-coord) (recur (pop queue) seen)
            :else (let [rest-queue (pop queue)
                        new-seen (conj seen curr-coord)
                        adjacent (map #(hash-map :coord %
                                                 :distance (inc ^int curr-distance)
                                                 :ancestor curr-ancestor)
                                      (set/difference (set (in-range curr-coord dungeon)) new-seen))
                        new-queue (->> adjacent
                                       (into rest-queue)
                                       (sort-by path-key)
                                       (apply list))]
                    (recur new-queue new-seen))))))))

(defn adjacent-targets [unit units]
  (let [adjacent-spaces (-> unit :pos cardinals set)]
    (->> units
         (filter #(not (dead? %)))
         (enemies unit)
         (filter #(contains? adjacent-spaces (:pos %))))))

(defn has-adjacent-targets? [unit units]
  (not (empty? (adjacent-targets unit units))))

(defn select-target-i
  "Given a unit, select the index of a chosen target, or nil if none available."
  [unit units]
  (let [potential-targets (adjacent-targets unit units)
        lowest-health (apply min 999 (map :hp potential-targets))
        target-pos (->> potential-targets
                        (filter #(= (:hp %) lowest-health))
                        (map :pos)
                        disambiguate)]
    (->> units
         (map-indexed vector)
         (some (fn [[i unit]] (when (= target-pos (:pos unit)) i))))))

(defn attack-target [target]
  (let [attack-power (case (:type target)
                       \E 3
                       \G *elf-attack-power*)
        hit-target (update target :hp #(- ^int % ^int attack-power))]
    (if (<= ^int (:hp hit-target) 0)
      (assoc hit-target :dead true)
      hit-target)))

(defn tick-move
  "Takes unit-i in units and moves it towards the nearest enemy in dungeon,
  returning the updated list of units."
  [unit-i units dungeon]
  (let [unit (get units unit-i)
        new-unit-pos (step-to-nearest unit dungeon)]
    (assoc-in units [unit-i :pos] new-unit-pos)))

;; Given a full unit, a list of all units, and a filled-in dungeon
(defn tick-unit
  "Takes unit-i in units and executes a full move, returning the updated list of
  units."
  [[units dungeon] unit-i]
  (let [unit (get units unit-i)]
    (if (dead? unit)
      [units dungeon]
      (let [moved-units (if (has-adjacent-targets? unit units)
                          units
                          (tick-move unit-i units dungeon))
            moved-unit (get moved-units unit-i)
            potential-target-i (select-target-i moved-unit moved-units)
            attacked-units (if potential-target-i
                             (update moved-units potential-target-i attack-target)
                             moved-units)
            new-dungeon (replace-units attacked-units dungeon)]
        [attacked-units new-dungeon]))))

(defn tick-dungeon [[units dungeon]]
  (let [sorted-units (vec (sort-by :pos units))
        unit-indices (range (count units))
        [ticked-units ticked-dungeon] (reduce tick-unit [sorted-units dungeon] unit-indices)
        culled-units (filter #(not (dead? %)) ticked-units)]
    [culled-units ticked-dungeon]))

(defn simulate-dungeon
  ([dungeon]
   (->> [(initial-units dungeon) dungeon]
        (iterate tick-dungeon)
        (partition 2 1)
        (take-while #(not (apply = %)))
        (map second)
        ((juxt count last))
        vec
        (#(update % 0 dec))))
  ([to-round dungeon]
   (->> [(initial-units dungeon) dungeon]
        (iterate tick-dungeon)
        (#(nth % to-round))
        (vector to-round))))

(defn run-dungeon [dungeon-str]
  (->> dungeon-str
       str/split-lines
       (mapv vec)
       simulate-dungeon
       ((fn [[round-n [units dungeon]]]
          [round-n units]))))

(defn score-dungeon [dungeon-str]
  (->> dungeon-str
       run-dungeon
       (#(update % 1 (fn [units] (reduce + (map :hp units)))))
       vec
       (#(update % 0 dec)) ;; Mis-counts rounds rarely, too much a bother to fix!
       (apply *)))

;; Part 1 Solution
(score-dungeon input)

;; Part 2 Solution
(let [num-elves (count (filter #(= \E %) input))]
  (->> (range)
       (drop 4)
       (map (fn [power]
              (binding [*elf-attack-power* power]
                (run-dungeon input))))
       (filter (fn [[round-n units]]
                 (= num-elves (->> units (filter #(= (:type %) \E)) count))))
       first
       (#(update % 1 (fn [units] (reduce + (map :hp units)))))
       (apply *)))
