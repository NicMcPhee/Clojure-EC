(ns ec.test-core 
  (:use ec.core clojure.test))

(deftest two-point-xo-test
  (are [parent1 parent2 point1 point2 expected] 
    (= (two-point-xo parent1 parent2 point1 point2) expected)
    [0 0 0 0] [1 1 1 1] 1 2 [0 1 0 0]
    [0 0 0 0 0 0] [1 1 1 1 1 1] 2 4 [0 0 1 1 0 0]
    [1 0 1 1 0 1] [0 1 1 0 1 1] 2 5 [1 0 1 0 1 1]
    [0 0 0 0 0 0] [1 1 1 1 1 1] 0 4 [1 1 1 1 0 0]
    [0 0 0 0 0 0] [1 1 1 1 1 1] 2 6 [0 0 1 1 1 1]
    [0 0 0 0 0 0] [1 1 1 1 1 1] 2 2 [0 0 0 0 0 0]
    [0 0 0 0 0 0] [1 1 1 1 1 1] -3 4 [1 1 1 1 0 0]
    [0 0 0 0 0 0] [1 1 1 1 1 1] 2 12 [0 0 1 1 1 1]
    [0 0 0 0 0 0] [1 1 1 1 1 1] -2 43 [1 1 1 1 1 1]))