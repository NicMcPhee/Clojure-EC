(ns ec.problems.count-ones
  (:use ec.core))

(defn count-ones
  [bit-string]
  (apply + bit-string))

(defn run-count-ones
  []
  (run-ga 100 1000 200 count-ones 2))