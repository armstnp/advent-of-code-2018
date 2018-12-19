(ns advent-of-code-2018.day19
  (:require [advent-of-code-2018.core :as core]
            [clojure.string :as str]
            [advent-of-code-2018.day16 :as parser]))

(set! *unchecked-math* :warn-on-boxed)

(def input (str/split-lines (core/read-input "day19.txt")))

(def op-code->parse-fn
  {"addr" parser/parse-addr
   "addi" parser/parse-addi
   "mulr" parser/parse-mulr
   "muli" parser/parse-muli
   "banr" parser/parse-banr
   "bani" parser/parse-bani
   "borr" parser/parse-borr
   "bori" parser/parse-bori
   "setr" parser/parse-setr
   "seti" parser/parse-seti
   "gtir" parser/parse-gtir
   "gtri" parser/parse-gtri
   "gtrr" parser/parse-gtrr
   "eqir" parser/parse-eqir
   "eqri" parser/parse-eqri
   "eqrr" parser/parse-eqrr})

(defn parse-instruction [line]
  (let [[_ op-code & arg-strs] (re-matches #"([a-z]{4}) (\d+) (\d+) (\d+)" line)
        args (map core/parse-int arg-strs)
        parse-fn (op-code->parse-fn op-code)]
    (apply parse-fn args)))

(defn step-instruction [{:keys [ip ip-reg registers instructions] :as device}]
  (if-let [instruction (get instructions ip)]
    (let [in-registers (assoc registers ip-reg ip)
          out-registers (parser/exec-op instruction in-registers)
          new-ip (inc ^int (get out-registers ip-reg))]
      (assoc device
             :ip new-ip
             :registers out-registers))
    (assoc device :halted true)))

(def ip-register (->> input
                      first
                      (re-matches #"#ip (\d)")
                      second
                      core/parse-int))
(def instructions (->> input
                       rest
                       (map parse-instruction)
                       vec))
(def initial-device {:ip 0 :ip-reg ip-register :registers [0 0 0 0 0 0] :instructions instructions})

;; Part 1 Solution
(->> initial-device
     (iterate step-instruction)
     (drop-while #(not (:halted %)))
     first
     :registers
     first)

;; 'Target' register index and algorithm inferred from cracking the assembly
;; Sums all the factors of some number (small when register 0 = 0, large when = 1)

(def target
  (->> initial-device
       (#(assoc-in % [:registers 0] 1))
       (iterate step-instruction)
       (drop 100)
       first
       :registers
       (#(get % 2))))

;; Part 2 Solution
(->> target
     inc
     (range 1)
     (filter #(zero? (mod ^int target ^int %)))
     (reduce +))

;; Interpreted Assembly:
;;
;; 00  JMP 17
;; 01  D = 1
;; 02  B = 1
;; 03  E = B * D
;; 04  E = (E = C) ? 1 : 0
;; 05  JMP E + 6
;; 06  JMP 8
;; 07  A += D
;; 08  B++
;; 09  E = (B > C) ? 1 : 0
;; 10  JMP E + 11
;; 11  JMP 3
;; 12  D += 1
;; 13  E = (D > C) ? 1 : 0
;; 14  JMP E + 15
;; 15  JMP 2
;; 16  HALT
;; 17  C += 2
;; 18  C *= C
;; 19  C *= 19
;; 20  C *= 11
;; 21  E += 8
;; 22  E *= 22
;; 23  E += 20
;; 24  C += E
;; 25  JMP A + 26
;; 26  JMP 1
;; 27  E = 27
;; 28  E *= 28
;; 29  E += 29
;; 30  E *= 30
;; 31  E += 14
;; 32  E *= 32
;; 33  C += E
;; 34  A = 0
;; 35  JMP 1
;;
;; Mungled BASIC:
;;
;; C = 1032
;; IF 'Hard Mode' flag is clear, THEN C = 10551432
;;
;; D = 1
;;
;; : Outer Loop
;; B = 1
;;
;; : Inner Loop
;; E = B * D
;;
;; IF E = C, THEN A += D
;;
;; B++
;; IF B <= C, THEN GOTO Inner Loop
;;
;; D++
;; IF D <= C, THEN GOTO Outer Loop
;;
;; HALT
;;
;; Interpreted Algorithm:
;;
;; Register roles
;; --------------
;; A: 'Result' ('Hard Mode' flag during initialization)
;; B: Factor 2
;; C: Target value (set on start)
;; D: Factor 1
;; E: Test Register (helper during initialization)
;;
;; Clojure Equivalent
;;
;; (let [target *
;;       factors (for [x (range 1 (inc target))
;;                     y (range 1 (inc target))
;;                     :when (= target (* x y))]
;;                 x)]
;;   (reduce + factors))
