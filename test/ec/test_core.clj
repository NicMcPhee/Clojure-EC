(ns ec.test-core 
  (:require [ec.primitives :as prim])
  (:use ec.core clojure.test))

(defn- two-point-assert
  [parent1 parent2 point1 point2 expected]
  (let [p1 (prim/bit-string-from-coll parent1)
        p2 (prim/bit-string-from-coll parent2)]
    (= (seq (two-point-xo p1 p2 point1 point2))
      (seq expected))))

(deftest two-point-xo-test
  (are [parent1 parent2 point1 point2 expected] 
    (two-point-assert parent1 parent2 point1 point2 expected)
    [0 0 0 0] [1 1 1 1] 1 2 [0 1 0 0]
    [0 0 0 0 0 0] [1 1 1 1 1 1] 2 4 [0 0 1 1 0 0]
    [1 0 1 1 0 1] [0 1 1 0 1 1] 2 5 [1 0 1 0 1 1]
    [0 0 0 0 0 0] [1 1 1 1 1 1] 0 4 [1 1 1 1 0 0]
    [0 0 0 0 0 0] [1 1 1 1 1 1] 2 6 [0 0 1 1 1 1]
    [0 0 0 0 0 0] [1 1 1 1 1 1] 2 2 [0 0 0 0 0 0]
    [0 0 0 0 0 0] [1 1 1 1 1 1] -3 4 [1 1 1 1 0 0]
    [0 0 0 0 0 0] [1 1 1 1 1 1] 2 12 [0 0 1 1 1 1]
    [0 0 0 0 0 0] [1 1 1 1 1 1] -2 43 [1 1 1 1 1 1]))

(deftest population-creation-test
  (let [pop-size 100
        bit-length 20
        counter (atom -1)
        fitness-fn (fn [_] (swap! counter inc))
        population (make-population pop-size bit-length fitness-fn)]
    (is (= (count population) pop-size))
    (is (apply = bit-length (map #(count (:bit-string %1)) population)))
    (is (= (map :fitness population) (range pop-size)))))
