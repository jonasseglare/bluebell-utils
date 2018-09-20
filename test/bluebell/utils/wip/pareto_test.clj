(ns bluebell.utils.wip.pareto-test
  (:require [bluebell.utils.wip.pareto :refer :all]
            [clojure.test :refer :all]))

(def empty-frontier (frontier (fn [[a b] [x y]] (and (< a x)
                                                     (< b y)))))

(deftest reduction-test
  (let [reds (map :elements
                  (reductions insert
                              empty-frontier
                              [[10 10]
                               [20 5]
                               [30 2]
                               [29 1]
                               [4 4]
                               [5 5]]))]
    (is (=

         reds

         [
          []

          [[10 10]]

          [[10 10] [20 5]]

          [[10 10] [20 5] [30 2]]

          [[10 10] [20 5] [29 1]]

          [[29 1] [4 4]]
          
          [[29 1] [4 4]]

          ]))))

