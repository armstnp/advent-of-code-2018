(ns advent-of-code-2018.day14)

(set! *unchecked-math* :warn-on-boxed)

(def num-recipes 880751)

(def initial-recipes [3 7])

(def int-offset (int \0))
(defn numeral->int [x]
  (- (int x) ^int int-offset))
(defn int->numerals [x]
  (mapv numeral->int (str x)))

(def gen-recipes
  (memoize
   (fn [a b]
     (map numeral->int (str (+ ^int a ^int b))))))

(defn step-elves [recipes a b]
  (let [a-recipe (get recipes a)
        b-recipe (get recipes b)
        new-recipes (into recipes (gen-recipes a-recipe b-recipe))
        new-a (mod (+ ^int a (inc ^int a-recipe)) (count new-recipes))
        new-b (mod (+ ^int b (inc ^int b-recipe)) (count new-recipes))]
    [new-recipes new-a new-b]))

(def needed-count (+ num-recipes 10))

; Part 1 Solution
(->> [initial-recipes 0 1]
     (iterate #(apply step-elves %))
     (map first)
     (drop-while #(< (count %) ^int needed-count))
     first
     (drop num-recipes)
     (take 10)
     (apply str))

; Part 2

(def pattern (int->numerals num-recipes))

; Only valid if the pattern is present at or just before the end of
; the recipe list
(defn trim-after-pattern [pattern recipes]
  (let [pattern-length (count pattern)
        tail-length (if (= (take-last pattern-length recipes) pattern)
                      pattern-length
                      (inc pattern-length))]
    (drop-last tail-length recipes)))

(defn step-elves-pattern [recipes a b pattern]
  (let [pattern-length (count pattern)]
    (loop [recipes recipes
           a a
           b b
           window (vec recipes)]
      (let [a-recipe (get recipes a)
            b-recipe (get recipes b)
            added-recipes (gen-recipes a-recipe b-recipe)
            new-recipes (into recipes added-recipes)
            new-recipe-length (count new-recipes)
            new-a (mod (+ ^int a (inc ^int a-recipe)) new-recipe-length)
            new-b (mod (+ ^int b (inc ^int b-recipe)) new-recipe-length)
            window-1 (subvec new-recipes
                             (max 0 (- new-recipe-length pattern-length))
                             new-recipe-length)
            window-2 (subvec new-recipes
                             (max 0 (- new-recipe-length (inc pattern-length)))
                             (dec new-recipe-length))]
        (if (or (= window-1 pattern) (= window-2 pattern))
          (trim-after-pattern pattern new-recipes)
          (recur new-recipes new-a new-b window))))))

; Part 2 solution
(count (step-elves-pattern initial-recipes 0 1 pattern))

