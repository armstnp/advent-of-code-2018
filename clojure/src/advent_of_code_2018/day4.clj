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

(def events
  (->> (str/split input #"\n")
       sort
       (map parse-event)))

(defn tally-guard-sleep [{:keys [curr-guard asleep-since] :as state}
                         [event-type event-arg]]
  (case event-type
    :guard (assoc state :curr-guard event-arg)
    :sleep (assoc state :asleep-since event-arg)
    :wake (update-in state
                     [:sleep-tally curr-guard]
                     (fn [total-slept] (+ (- event-arg asleep-since)
                                          (if (nil? total-slept) 0 total-slept))))))

(def sleepiest-guard
  (->> events
       (reduce tally-guard-sleep {})
       :sleep-tally
       (sort-by second)
       reverse
       first
       first))

(defn tally-guard-minutes [{:keys [selected-guard curr-guard minute-tally asleep-since] :as state}
                           [event-type event-arg]]
  (case event-type
    :guard (assoc state :curr-guard event-arg)
    :sleep (assoc state :asleep-since event-arg)
    :wake (if (not= curr-guard selected-guard)
            state
            (update state
                    :minute-tally
                    (fn [curr-tally] (reduce #(update %1 %2 inc)
                                             curr-tally
                                             (range asleep-since event-arg)))))))

(def init-minutes (zipmap (range 0 60) (repeat 0)))

(def sleepiest-minute
  (->> events
       (reduce tally-guard-minutes {:selected-guard sleepiest-guard :minute-tally init-minutes})
       :minute-tally
       (sort-by second)
       reverse
       first
       first))

; Part 1 answer
(* sleepiest-guard sleepiest-minute)

(defn tally-every-guard-minutes [{:keys [curr-guard minute-tallies asleep-since] :as state}
                                 [event-type event-arg]]
  (case event-type
    :guard (assoc state :curr-guard event-arg)
    :sleep (assoc state :asleep-since event-arg)
    :wake (update-in state
                     [:minute-tallies curr-guard]
                     (fn [curr-tally] (reduce #(update %1 %2 (fnil inc 0))
                                              curr-tally
                                              (range asleep-since event-arg))))))

(defn get-biggest-minute [minute-tally]
  (->> minute-tally (sort-by second) reverse first))

; Solution 2
(->> events
     (reduce tally-every-guard-minutes {})
     :minute-tallies
     (map (fn [[guard tally]] [guard (get-biggest-minute tally)]))
     (sort-by (comp second second))
     reverse
     first
     ((juxt first (comp first second)))
     (apply *))
