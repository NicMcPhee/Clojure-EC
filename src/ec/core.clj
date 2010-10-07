;;; TO-DOs
;;; Look at parallelizing the creation of new generations so we can use multi-cores.

(ns ec.core
  (:use ec.primitives))

(defn- choices-fn
  "Return a fn for use in masked-xo that implements the selection logic
described in the two-point-xo documentation."
  [j k]
  (fn [n]
    (cond
      (< n j) true
      (< n k) false
      :else true)))

(defn two-point-xo
  "Performs standard two point crossover on the given sequences.
   The resulting child will take its first j elements from first-parent,
   elements j<=i<k from the second parent, and elements k<=i from
   the first parent again."
  ([first-parent second-parent]
    (let [length (count first-parent)
          first-point (rand-int length)
          second-point (rand-int length)]
      (two-point-xo first-parent second-parent first-point second-point)))
  ([first-parent second-parent j k]
    (masked-xo (choices-fn j k) first-parent second-parent)))

(defn point-mutation
  "Randomly flip the bits in the given sequence with uniform probability."
  ([parent mutation-rate]
   (let [random-bits (fn [_] (< (rand) mutation-rate))]
     (masked-point-mutation random-bits parent)))
  ([parent]
    (point-mutation parent (/ 1.0 (count parent)))))

(defn two-point-xo-point-mutation
  [first-parent second-parent]
  (let [length (count first-parent)
        j (rand-int length)
        k (+ j (rand-int (- length j)))
        choices (choices-fn j k)
        mut-rate (/ 1.0 length)
        rnd (fn [_] (< (rand) mut-rate))]
    (masked-xo-point-mutation choices first-parent second-parent rnd)))

(defn tournament-selection
  "Generates a tournament with no duplication having the specified size and
   returns the individual from that tournament with the largest fitness."
  [population tournament-size]
  (let [rnd-fn #(rand-int (count population))
        candidates (map population (repeatedly tournament-size rnd-fn))]
    (apply max-key :fitness candidates)))

(defn make-individual
  "Create an individual having the specified number of randomly generated bits using 
   the specified fitness function."
  [num-bits fitness-function]
  (let [random-bits (random-bit-string num-bits)
        fitness (fitness-function random-bits)]
    {:bit-string random-bits
     :fitness fitness}))

(defn make-population
  "Create a population of randomly created individuals having the specified number of bits
   and using the given fitness function."
  [num-individuals num-bits fitness-function]
  (vec (repeatedly num-individuals #(make-individual num-bits fitness-function))))

(defn child
  [population fitness-function tournament-size]
  (let [first-parent (:bit-string (tournament-selection population tournament-size))
        second-parent (:bit-string (tournament-selection population tournament-size))
        mut-result (two-point-xo-point-mutation first-parent second-parent)
        result {:bit-string mut-result :fitness (fitness-function mut-result)}]
    result))

(defn next-generation
  [num-individuals fitness-function tournament-size population]
  (vec (map (fn [_] (child population fitness-function tournament-size)) (range num-individuals))))

(defn count-ones
  [bit-string]
  (apply + bit-string))

(defn run-ga
  [num-gens num-individuals num-bits fitness-function tournament-size]
  (let [initial-pop (make-population num-individuals num-bits fitness-function)]
    (take num-gens (iterate #(next-generation num-individuals fitness-function tournament-size %1) initial-pop))))

(defn do-run-ga
  []
  (let [pops (run-ga 100 1000 50 count-ones 2)]
    (map #(apply max (map :fitness %)) pops)))
