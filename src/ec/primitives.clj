;;;; Performant primitives for a GA system
(ns ec.primitives)

(defn bit-string-from-coll
  "Create a bit string from coll"
  [coll]
  (int-array coll))

(defn random-bit-string
  "Create a random bit string n bits long"
  [n]
  (let [arr (int-array n)]
    (dotimes [i n]
      (aset-int arr i (rand-int 2)))
    arr))

(defn- masked-xo!
  "Like masked-xo, but modify and return arr"
  [^ints arr mask ^ints first-parent ^ints second-parent]
  (dotimes [n (count arr)]
    (aset-int arr n (aget (if (mask n) first-parent second-parent) n)))
  arr)

(defn masked-xo
  "Perform masked crossover on first-parent and second-parent.  mask is a fn
that takes takes and index and returns true if the corresponding bit should
be taken from first-parent, otherwise second-parent"
  [mask ^ints first-parent ^ints second-parent]
  (doto (int-array (count first-parent))
    (masked-xo! mask first-parent second-parent)))

(defn- masked-point-mutation!
  "Like masked-point-mutation, but modify and return arr"
  [^ints arr mask ^ints parent]
  (dotimes [n (count arr)]
    (let [x (aget parent n)]
      (aset-int arr n (if (mask n) (- 1 x) x))))
  arr)

(defn masked-point-mutation
  "Perform point mutation on parent.  mask is a fn that takes an index; if it
returns true, the corresponding location has a bit flipped"
  [mask ^int parent]
  (doto (int-array (count parent))
    (masked-point-mutation! mask parent)))

(defn masked-xo-point-mutation
  "A combination of masked-xo and masked-point-mutation.  Operations happen in
that order"
  [mask-xo first-parent second-parent mask-mut]
  (let [xo (masked-xo! (int-array (count first-parent))
             mask-xo first-parent second-parent)]
    (masked-point-mutation! xo mask-mut xo)
    xo))
