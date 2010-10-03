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
  [first-parent second-parent j k]
  (masked-xo (choices-seq j k) first-parent second-parent))

(defn masked-point-mutation
  "Use the given mask to perform point mutation on a sequence. Where
   elements of the mask are 1, the corresponding bit will be flipped (i.e.,
   a 1 will be come a 0 and a 0 will become a 1); when the element is 0
   it will remain unchanged."
  [parent mask]
  (map bit-xor mask parent))

(defn point-mutation
  "Randomly flip the bits in the given sequence with uniform probability."
  [parent]
  (let [random-bits (repeatedly #(rand-int 2))]
    (masked-point-mutation parent random-bits)))