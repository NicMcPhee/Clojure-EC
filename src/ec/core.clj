;;; TO-DOs
;;; Improve performance of tournament-selection (which profiling shows really sucks).
;;; Look at parallelizing the creation of new generations so we can use multi-cores.

(ns ec.core)

(defn- choices-seq
  "Generate a sequence of boolean values indicating whether a particular
   element in a crossover event comes from the first or the second parent."
  [j k]
  (concat 
    (repeat j true)
    (repeat (- k (max 0 j)) false)
    (repeat true)))

(defn- masked-xo
  "Use the given mask to perform crossover between two sequences. Where
   elements of the mask are true, the corresponding item will come from
   first-parent; when the element is false it will come from the second-parent."
  [mask first-parent second-parent]
  (map #(if %1 %2 %3) mask first-parent second-parent))

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
    (masked-xo (choices-seq j k) first-parent second-parent)))

(defn masked-point-mutation
  "Use the given mask to perform point mutation on a sequence. Where
   elements of the mask are 1, the corresponding bit will be flipped (i.e.,
   a 1 will be come a 0 and a 0 will become a 1); when the element is 0
   it will remain unchanged."
  [parent mask]
  (map bit-xor mask parent))

(defn random-bits
  "Generate a sequence of random bits. If no argument is provided, an infinite
   lazy sequence is generated. If a desired size is provided, then a sequence
   having the specified number of bits is provided."
  ([]
    (repeatedly #(rand-int 2)))
  ([num-bits]
    (take num-bits (random-bits))))

(defn point-mutation
  "Randomly flip the bits in the given sequence with uniform probability."
  ([parent mutation-rate]
   (let [random-bits (repeatedly #(if (< (rand) mutation-rate) 1 0))]
    (masked-point-mutation parent random-bits)) )
  ([parent]
    (point-mutation parent (/ 1.0 (count parent)))))

(defn tournament-selection
  "Generates a tournament with no duplication having the specified size and
   returns the individual from that tournament with the largest fitness."
  [population tournament-size]
  (let
    [candidates (take tournament-size (shuffle population))]
    (apply max-key :fitness candidates)))

(defn make-individual
  "Create an individual having the specified number of randomly generated bits using 
   the specified fitness function."
  [num-bits fitness-function]
  (let 
    [random-bits (repeatedly num-bits #(rand-int 2))
     fitness (fitness-function random-bits)]
    {:bit-string (vec random-bits)
     :fitness fitness}))

(defn make-population
  "Create a population of randomly created individuals having the specified number of bits
   and using the given fitness function."
  [num-individuals num-bits fitness-function]
  (repeatedly num-individuals #(make-individual num-bits fitness-function)))

(defn child
  [population fitness-function tournament-size]
  (let [first-parent (:bit-string (tournament-selection population tournament-size))
        second-parent (:bit-string (tournament-selection population tournament-size))
        xo-result (two-point-xo first-parent second-parent)
        mutation-result (point-mutation xo-result)
        result {:bit-string (vec mutation-result) :fitness (fitness-function mutation-result)}]
    result))

(defn next-generation
  [num-individuals fitness-function tournament-size population]
  (map (fn [_] (child population fitness-function tournament-size)) (range num-individuals)))

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
