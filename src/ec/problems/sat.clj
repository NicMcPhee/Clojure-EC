(ns ec.problems.sat
  (:import java.util.Scanner))

(defn- satisfies-variable?
  [bit-string variable]
  (let [position (Math/abs variable)
        value (nth bit-string (dec position))
        expected-value (if (< variable 0) 0 1)]
    (= value expected-value)))

(defn- eval-clause
  [bit-string clause]
  (if (some #(satisfies-variable? bit-string %) clause)
    1
    0))

(defn eval-sat
  [problem-instance]
  (fn [bit-string]
    (reduce + (map #(eval-clause bit-string %) problem-instance))))

;;; TODO Finish this so it parses a 3-sat instance in filename.
(defn read-instance
  [filename]
  (with-open [scanner (Scanner. filename)]
    nil))