(ns bluebell.utils.wip.data-factors-test
  (:require [bluebell.utils.wip.data-factors :refer :all]
            [clojure.test :refer :all]))

(deftest factor-test
  (let [result (factorize {:x [3 [1 2] [4 5]] :y [3 [1 2] [4 5]] :z [4 5]})]
    (is (map? result))
    (is (every? keyword? (keys result)))
    (is (= 3 (count result)))))
