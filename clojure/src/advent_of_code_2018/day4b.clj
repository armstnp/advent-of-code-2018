(ns advent-of-code-2018.day4
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(def input (core/read-input "day4.txt"))

(def shift-start-re #"\[.+\] Guard #(\d+) begins shift")
(def falls-asleep-re #"\[.+:(\d+)\] falls asleep")
(def wakes-up-re #"\[.+:(\d+)\] wakes up")

(defn parse-re-int [re s]
  (->> s
       (re-matches re)
       second
       core/parse-int))

(defn parse-event [event]
  (cond
    (str/includes? event "Guard")
    [:guard (parse-re-int shift-start-re event)]

    (str/includes? event "falls")
    [:sleep (parse-re-int falls-asleep-re event)]

    :else
    [:wake (parse-re-int wakes-up-re event)]))

(defn handle-event [{:keys [curr-guard guard-minute-tallies asleep-since] :as state}
                    [event-type event-arg]]
  (case event-type
    :guard (assoc state :curr-guard event-arg)
    :sleep (assoc state :asleep-since event-arg)
    :wake (update-in state
                     [:guard-minute-tallies curr-guard]
                     (fn [curr-tally] (reduce #(update %1 %2 (fnil inc 0))
                                              curr-tally
                                              (range asleep-since event-arg))))))

(def guard first)
(def minute-and-tally second)
(def minute (comp first minute-and-tally))
(def tally (comp second minute-and-tally))

(defn get-biggest-minute [minute-tally]
  (first (sort-by-desc second minute-tally)))

(defn sort-by-desc [keyfn coll]
  (sort-by (comp - keyfn) coll))

(def max-by (comp first sort-by-desc))

(def guard-minute-tallies
  (->> (str/split input #"\n")
       sort
       (map parse-event)
       (reduce handle-event {})
       :guard-minute-tallies))

; Part 1 answer
(->> guard-minute-tallies
     (max-by #(reduce + (vals (second %))))
     ((juxt guard (comp first get-biggest-minute minute-and-tally)))
     (apply *))

; Solution 2
(->> guard-minute-tallies
     (map (fn [[guard tally]] [guard (get-biggest-minute tally)]))
     (max-by tally)
     ((juxt guard minute))
     (apply *))
