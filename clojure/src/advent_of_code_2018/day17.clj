(ns advent-of-code-2018.day17
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(def input (str/split (core/read-input "day17.txt") #"\n"))

(defn parse-vert-vein [line]
  (let [[_ & matches] (re-matches #"x=(\d+), y=(\d+)\.\.(\d+)" line)
        [x y-start y-end] (map core/parse-int matches)]
    (map #(vector x %) (range y-start (inc ^int y-end)))))

(defn parse-horiz-vein [line]
  (let [[_ & matches] (re-matches #"y=(\d+), x=(\d+)\.\.(\d+)" line)
        [y x-start x-end] (map core/parse-int matches)]
    (map #(vector % y) (range x-start (inc ^int x-end)))))

(defn parse-vein [line]
  (if (= \x (first line))
    (parse-vert-vein line)
    (parse-horiz-vein line)))

(defn build-scan [input]
  (let [scan-map (->> input
                      (mapcat parse-vein)
                      (map #(vector % \#))
                      (into {})
                      (#(assoc % [500 0] \+ [500 1] \|)))
        y-bound (apply max (map second (keys scan-map)))]
    {:scan-map scan-map :y-bound y-bound}))

(defn flowing-water? [x] (= x \|))
(defn settled-water? [x] (= x \~))
(defn water? [x] (or (flowing-water? x) (settled-water? x)))
(defn solid? [x] (or (= x \#) (settled-water? x)))
(defn free? [x] (not (solid? x)))

(defn can-flow-down? [[x y] {:keys [scan-map y-bound]}]
  (and (free? (get scan-map [x (inc ^int y)]))
       (<= ^int y ^int y-bound)))

(defn can-flow-sideways? [[x y :as coord] {:keys [scan-map]}]
  (solid? (get scan-map [x (inc ^int y)])))

(defn flow-down [[x y :as coord] scan]
  (if (can-flow-down? coord scan)
    [:flowed (assoc-in scan [:scan-map [x (inc ^int y)]] \|)]
    [:stayed scan]))

;; Available when you cannot flow down
(defn flow-sideways [[x y :as coord] dx {:keys [scan-map] :as scan}]
  (let [[mx my :as move-coord] [(+ ^int x ^int dx) y]]
    (if (solid? (get scan-map move-coord))
      [:stayed scan]
      (let [flowed-sideways (assoc-in scan [:scan-map move-coord] \|)
            [flowed-down-state flowed-down-scan :as flowed-down] (flow-down move-coord flowed-sideways)]
        (if (= :flowed flowed-down-state)
          flowed-down
          (recur move-coord dx flowed-sideways))))))

(defn settle
  ([[x y :as coord] scan]
   (settle [(inc ^int x) y] 1 (settle coord -1 scan)))
  ([[x y :as coord] dx {:keys [scan-map] :as scan}]
   (if (solid? (get scan-map coord))
     scan
     (recur [(+ ^int x ^int dx) y] dx (assoc-in scan [:scan-map coord] \~)))))

(defn flow-both-sides [coord scan]
  (let [[left-flow-state left-flow-scan] (flow-sideways coord -1 scan)
        [right-flow-state right-flow-scan :as new-scan] (flow-sideways coord 1 left-flow-scan)]
    (if (= :stayed left-flow-state right-flow-state)
      [:stayed (settle coord right-flow-scan)]
      [:flowed right-flow-scan])))

(defn flow [coord scan]
  (let [[flow-down-state flowed-down-scan] (flow-down coord scan)]
    (if (= :stayed flow-down-state)
      (flow-both-sides coord flowed-down-scan)
      [:flowed flowed-down-scan])))

;; Raster

(defn raster-flow [[row-flow-state scan] coord]
  (let [[flow-state flow-scan] (flow coord scan)
        new-flow-state (if (= row-flow-state flow-state :flowed)
                         :flowed
                         :stayed)]
    [new-flow-state flow-scan]))

(defn raster [y {:keys [scan-map] :as scan}]
  (let [min-x (->> scan-map keys (map first) (apply min))
        max-x (->> scan-map keys (map first) (apply max) int inc)]
    (->> (range min-x max-x)
         (map #(vector % y))
         (filter #(flowing-water? (get scan-map %)))
         (reduce raster-flow [:flowed scan]))))

(defn ratchet-raster [y {:keys [y-bound] :as scan}]
  (if (> y y-bound)
    scan
    (let [[raster-state raster-scan] (raster y scan)]
      (recur (if (= raster-state :stayed) (dec y) (inc y))
             raster-scan))))

(defn count-water [min-y water-tiles {:keys [scan-map y-bound] :as scan}]
  (->> scan-map
       (filter (fn [[[_ y] _]] (and (>= y min-y) (<= y y-bound))))
       (map second)
       (filter water-tiles)
       count))

(def completed-flow
  (->> input
       build-scan
       (ratchet-raster 0)))

(def original-min-y
  (->> input
       (mapcat parse-vein)
       (map second)
       (apply min)))

;; Part 1 Solution
(->> completed-flow
     (#(do (draw-scan-map %) %))
     (count-water original-min-y water?))

;; Part 2 Solution
(->> completed-flow
     (count-water original-min-y settled-water?))

;; Helpers for visualization

(defn draw-scan-map [{:keys [scan-map y-bound]}]
  (let [min-x (->> scan-map keys (map first) (apply min))
        max-x (->> scan-map keys (map first) (apply max) int inc)
        width (- max-x min-x)
        height (inc y-bound)
        header (str "P3\n" width " " height "\n" "1\n")
        colors {\. "0 0 0"
                \# "1 1 0"
                \| "0 1 1"
                \~ "0 0 1"
                \+ "1 0 0"}
        body (str/join
              (->> (range (inc ^int y-bound))
                   (map (fn [y]
                          (->> (range min-x max-x)
                               (map #(colors (or (scan-map [% y]) \.)))
                               (str/join " "))))
                   (str/join "\n")))
        filename "flow.ppm"]
    (spit filename header)
    (spit filename body :append true)))
