(ns advent-of-code-2018.day21
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2018.day19 :as parser]))

(set! *unchecked-math* :warn-on-boxed)

(def input (str/split-lines (core/read-input "day21.txt")))

(def ip-register
  (->> input
       first
       (re-matches #"#ip (\d)")
       second
       core/parse-int))

(def instructions
  (->> input
       rest
       (map parser/parse-instruction)
       vec))

(def initial-device
  {:ip 0
   :ip-reg ip-register
   :registers [0 0 0 0 0 0]
   :instructions instructions})

; Part 1 Solution
(->> initial-device
     (iterate parser/step-instruction)
     (filter #(= 29 (:ip %)))
     first
     :registers
     (#(get % 3)))

;; D = 212115
;; D is calculated based on the bottom byte of C
;; This means the D has finite possible values, apply pigeon-hole!
;; If we see a value we've seen before, we've gone too far!
;; Recreating the algorithm here for efficiency:
;;
;; Bitwise Verification:
;; IF 0x7B & 0x1C8 = 0x48
;; THEN GOTO MAIN
;; ELSE LOOP INFINITELY
;;
;; Main:
;; D = 0
;;
;; Generate Hash:
;; C = 0x10000 | D
;; D = 0xCB338
;;
;; Next Hash Attempt:
;; D = D + bottom byte of C
;; Trim D to 3 bytes, multiply by 0x1016B, trim again
;;
;; IF C < 256
;; THEN GOTO Hash-Test
;;
;; C = FLOOR(C/256)
;; GOTO Next Hash Attempt
;;
;; Hash-Test:
;; IF A = D
;; THEN HALT
;; ELSE GOTO Generate Hash 

(defn hash-d [c d]
  (let [bottom-c (bit-and ^long c 0xFF)
        next-d (->> ^long d
                    (+ bottom-c)
                    (bit-and 0xFFFFFF)
                    (* 65899)
                    (bit-and 0xFFFFFF))]
    (if (> 256 ^int c)
      next-d
      (recur (long (Math/floor (/ (double c) 256.0))) next-d))))

(defn tick-manual [d]
  (let [c (bit-or 0x10000 ^long d)
        d 832312]
    (hash-d c d)))

;; Part 2 Solution
(->> 0
     (iterate tick-manual)
     (drop 1)
     (reductions conj #{})
     (partition 2)
     (take-while #(apply not= %))
     last
     reverse
     (apply set/difference)
     first)
