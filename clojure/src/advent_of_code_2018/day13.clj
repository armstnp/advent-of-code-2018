(ns advent-of-code-2018.day13
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(def input (str/split (core/read-input "day13.txt") #"\n"))
(def grid (mapv vec input))

(defn get-cell [grid [x y]]
  (get-in grid [y x]))

(def dir->offset
  {:left  [-1  0]
   :right [ 1  0]
   :up    [ 0 -1]
   :down  [ 0  1]})

(def update-next-turn
  {:turn-left   :go-straight
   :go-straight :turn-right
   :turn-right  :turn-left})

(def intersection-turn
  {[:left  :turn-left]   :down
   [:down  :turn-left]   :right
   [:right :turn-left]   :up
   [:up    :turn-left]   :left
   [:left  :go-straight] :left
   [:down  :go-straight] :down
   [:right :go-straight] :right
   [:up    :go-straight] :up
   [:left  :turn-right]  :up
   [:down  :turn-right]  :left
   [:right :turn-right]  :down
   [:up    :turn-right]  :right})

(def curve
  {[:left  \/] :down
   [:up    \/] :right
   [:down  \/] :left
   [:right \/] :up
   [:left  \\] :up
   [:up    \\] :left
   [:down  \\] :right
   [:right \\] :down})

(def initial-dir
  {\^ :up
   \v :down
   \< :left
   \> :right})

(defn move [pos dir]
  (mapv + pos (dir->offset dir)))

(def straight? #{\- \| \^ \v \< \>})
(def curve? #{\\ \/})
(def intersection? #{\+})
(def cart? #{\^ \v \< \>})

(defn parse-carts [grid]
  (for [y (range (count grid))
        x (range (count (get grid y)))
        :let [pos [x y]
              cell (get-cell grid pos)]
        :when (cart? (get-cell grid pos))]
    {:pos pos
     :dir (initial-dir cell)
     :next-turn :turn-left}))

(defn apply-track [{:keys [dir next-turn]} track]
  (cond
    (straight? track) [dir next-turn]
    (curve? track)    [(curve [dir track]) next-turn]
    (intersection? track) [(intersection-turn [dir next-turn])
                           (update-next-turn next-turn)]))

(defn tick-cart [grid {:keys [pos dir] :as cart}]
  (let [next-pos (move pos dir)
        next-track (get-cell grid next-pos)
        [new-dir new-next-turn] (apply-track cart next-track)]
    {:pos next-pos
     :dir new-dir
     :next-turn new-next-turn}))

(defn collisions [carts]
  (->> carts
       (map :pos)
       frequencies
       (filter #(> ^int (second %) 1))
       (map first)))

(defn tick-carts [grid carts]
  (let [sorted-carts (vec (sort-by :pos carts))]
    (loop [carts sorted-carts
           index 0]
      (if (= index (count carts))
        [:ok carts]
        (let [new-carts (assoc carts index (tick-cart grid (get carts index)))
              colls (collisions new-carts)
              any-collided? (not (empty? colls))]
          (if any-collided?
            [:coll colls]
            (recur new-carts (inc index))))))))

(def carts (parse-carts grid))

; Part 1 Solution
(->> [:ok carts]
     (iterate #(tick-carts grid (second %)))
     (drop-while #(= :ok (first %)))
     first
     second)

; Part 2
(defn tick-carts-with-helpers [grid carts]
  (let [sorted-carts (vec (sort-by :pos carts))]
    (loop [carts sorted-carts
           index 0]
      (if (>= index (count carts))
        carts
        (let [new-carts (assoc carts index (tick-cart grid (get carts index)))
              colls (set (collisions new-carts))
              safe-carts (vec (filter #(not (colls (:pos %))) new-carts))
              index-offset (->> new-carts
                                (map-indexed #(and (colls (:pos %2))
                                                   (<= %1 index)))
                                (filter identity)
                                count)]
            (recur safe-carts (+ index 1 (- index-offset))))))))

; Part 2 Solution
(->> carts
     (iterate #(tick-carts-with-helpers grid %))
     (drop-while #(> (count %) 1))
     first
     first
     :pos)
