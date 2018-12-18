(ns advent-of-code-2018.day18
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(def input (str/split (core/read-input "day18.txt") #"\n"))

(defn initial-forest []
  (let [forest-map (map seq input)
        width (count (first forest-map))
        height (count forest-map)
        forest-array (into-array Character/TYPE (flatten forest-map))]
    {:forest-map forest-array
     :fill-map (aclone forest-array)
     :width width
     :height height}))

(def d-adjacent
  [[1 -1]
   [1 0]
   [1 1]
   [0 1]
   [0 -1]
   [-1 -1]
   [-1 0]
   [-1 1]])

(defn in-bounds? [[y x] {:keys [width height]}]
  (and (>= ^int x 0)
       (< ^int x ^int width)
       (>= ^int y 0)
       (< ^int y ^int height)))

(defn get-coord [{:keys [forest-map width]} [y x]]
  (aget ^chars forest-map (+ ^int x (* ^int width ^int y))))

(defn set-coord [{:keys [fill-map width]} [y x] v]
  (aset ^chars fill-map (+ ^int x (* ^int width ^int y)) ^char v))

(defn adjacent-tally [coord forest]
  (->> d-adjacent
       (map #(mapv + coord %))
       (filter #(in-bounds? % forest))
       (map #(get-coord forest %))
       frequencies
       (merge {\. 0 \| 0 \# 0})))

(defn tick-cell [coord forest]
  (let [curr-type (get-coord forest coord)
        adjacents (adjacent-tally coord forest)
        next-type (case curr-type
                    \. (if (>= ^int (get adjacents \|) 3) \| \.)
                    \| (if (>= ^int (get adjacents \#) 3) \# \|)
                    \# (if (and (>= ^int (get adjacents \#) 1)
                                (>= ^int (get adjacents \|) 1)) \# \.))]
    (set-coord forest coord next-type)))

(defn tick-forest [{:keys [forest-map fill-map width height] :as forest}]
  (doseq [x (range 0 width)
          y (range 0 height)]
    (tick-cell [y x] forest))
  (assoc forest :forest-map fill-map :fill-map forest-map))

(defn tally-all [{:keys [forest-map]}]
  (frequencies (vec forest-map)))

(->> (initial-forest)
     (iterate tick-forest)
     (drop 10)
     first
     tally-all
     (#(dissoc % \.))
     vals
     (apply *))

(def first-i-and-cycle-size
  (->> (initial-forest)
       (iterate tick-forest)
       (map-indexed #(vector %1 (vec (aclone (:forest-map %2)))))
       (reduce (fn [{:keys [seen patts]} [new-patt-i new-patt-a :as new-patt]]
                 (if (contains? seen new-patt-a)
                   (let [[old-patt-i] (first (filter #(= (second %) new-patt-a) patts))]
                     (reduced [old-patt-i (- new-patt-i old-patt-i)]))
                   {:seen (conj seen new-patt-a)
                    :patts (conj patts new-patt)}))
               {:seen #{}
                :patts []})))

(def target-minute 1000000000)

(let [[first-i cycle-size] first-i-and-cycle-size]
  (->> (initial-forest)
       (iterate tick-forest)
       (drop first-i)
       (drop (mod (- target-minute first-i) cycle-size))
       first
       tally-all
       (#(dissoc % \.))
       vals
       (apply *)))
