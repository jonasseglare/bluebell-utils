(ns bluebell.utils.wip.timelog-test
  (:require [bluebell.utils.wip.timelog :refer :all]
            [clojure.test :refer :all]))

(deftest try-timelog
  (let [a (timelog)
        _ (Thread/sleep 10)
        b (log a "Kattskit")]
    (is (= (first (second b))
           "Kattskit"))
    (let [c (offset-normalize b)]
      (is (zero? (second (first c))))
      ;(disp c)
      )))
