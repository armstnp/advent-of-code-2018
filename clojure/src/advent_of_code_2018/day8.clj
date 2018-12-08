(ns advent-of-code-2018.day8
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(def input (core/read-input "day8.txt"))

(def flat-tree (map core/parse-int (str/split (str/trim input) #" ")))

(defn unpack-children [[new-child rest-els] children]
  [(conj children new-child) rest-els])

(defn traverse-node [elements]
  (let [[n-children n-meta & rest-els] elements
        [children rest-els] (loop [[children els] [[] rest-els]
                                   children-left n-children]
                              (if (zero? children-left)
                                [children els]
                                (recur (unpack-children (traverse-node els) children)
                                       (dec children-left))))
        [metadata rest-els] (split-at n-meta rest-els)]
    [{:children children
      :metadata metadata}
     rest-els]))

(def tree (traverse-node flat-tree))

;Part 1

(defn sum-metadata [node]
  (let [self-sum (reduce + (:metadata node))
        children-sum (reduce + (map sum-metadata (:children node)))]
    (+ self-sum children-sum)))

; Part 1 Solution
(reduce + (map sum-metadata tree))

; Part 2

(defn index-child [children index]
  (when (and (not (zero? index))
             (<= index (count children)))
    (nth children (dec index))))

(defn value-node [{:keys [children metadata]}]
  (if (empty? children)
    (reduce + metadata)
    (->> metadata
         (map #(index-child children %))
         (filter (comp not nil?))
         (map value-node)
         (reduce +))))

; Part 2 Solution
(reduce + (map value-node tree))
