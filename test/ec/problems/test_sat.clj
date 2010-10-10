(ns ec.problems.test-sat
  (:use ec.problems.sat clojure.test))

(deftest eval-sat-test
  (let [problem-instance [[1 2 3] [-1 2 -3] [1 2 -3] [1 -2 3]]
        eval-function (eval-sat problem-instance)]
    (are [bit-string expected]
      (= (eval-function bit-string) expected)
      [0 1 0] 3
      [1 1 0] 4
      [1 1 1] 4
      [1 0 1] 3
      [1 0 0] 4)))
