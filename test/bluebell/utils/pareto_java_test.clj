(ns bluebell.utils.pareto-java-test
  (:require [clojure.test :refer :all])
  (:import [bluebell.utils ParetoFrontier IDominates
            MemoizedDominates]))

(defn less-pair? [[a b]]
  (< a b))

(defn vector-dominates [a b]
  (let [ab (map vector a b)
        ba (map vector b a)]
    (and (some less-pair? ab)
         (not (some less-pair? ba)))))

(defn vector-dominates-proxy []
  (proxy [IDominates] []
    (dominates [a b]
      (if (not (vector? a))
        (throw (ex-info "Not vector a" {})))
      (if (not (vector? b))
        (throw (ex-info "Not vector b" {})))
      (assert (vector? a))
      (assert (vector? b))
      (boolean (vector-dominates a b)))))

(defn memoized-dominates [dom]
  (MemoizedDominates. dom))

(defn run-tests-with-dom [dom]
  (let [f (ParetoFrontier. dom)]
    (is (empty? (.getElements f)))
    (.insert f [1 2])
    (is (= (.getElements f)
           [[1 2]]))
    (.insert f [0 3])
    (is (= (.getElements f)
           [[1 2] [0 3]]))
    (.insert f [-1 3])
    (is (= (.getElements f)
           [[1 2] [-1 3]]))
    (.insert f [-1 -1])
    (is (= (.getElements f)
           [[-1 -1]]))
    (.insert f [-2 0])
    (.insert f [-3 1])
    (.insert f [-4 2])
    (is (= (.getElements f)
           [[-1 -1]
            [-2 0]
            [-3 1]
            [-4 2]]))))

(deftest pareto-experiment
  (run-tests-with-dom (vector-dominates-proxy))
  (let [a (vector-dominates-proxy)
        m (memoized-dominates a)]
    (run-tests-with-dom m)
    ;(.disp m)
    ))
