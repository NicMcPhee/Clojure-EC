(ns ec.test-primitives
  (:use
    ec.primitives
    clojure.test))

(deftest test-bit-string-from-coll
  (are [coll] (= (seq (bit-string-from-coll coll)) (seq coll))
    []
    [1 1 0 1 1]))

(defn- convert-mask
  [mask]
  (vec (map #(= % 1) mask)))

(deftest test-masked-xo
  (are [mask p1 p2 expected] (let [par1 (bit-string-from-coll p1)
                                   par2 (bit-string-from-coll p2)]
                               (= (seq (masked-xo (convert-mask mask) par1 par2))
                                 (seq expected)))
    [0 0 1 1] [0 0 0 0] [1 1 1 1] [1 1 0 0]
    [0 0 1 0] [0 0 0 0] [1 1 1 1] [1 1 0 1]
    [0 0 1 1] [1 0 1 0] [1 1 1 1] [1 1 1 0]
    [0 0 1 1] [1 0 1 0] [0 1 1 1] [0 1 1 0]))

(deftest masked-point-mutation-test
  (are [parent mask expected] (let [par (bit-string-from-coll parent)
                                    mut (masked-point-mutation (convert-mask mask) par)]
                                (= (seq mut) (seq expected)))
    [0 0 0 0 0 0] [0 1 1 0 0 1] [0 1 1 0 0 1]
    [1 1 1 1 1 1] [0 1 1 0 0 1] [1 0 0 1 1 0]
    [0 1 1 1 0 0] [1 0 1 0 1 1] [1 1 0 1 1 1]))
