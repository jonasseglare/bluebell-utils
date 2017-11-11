(ns bluebell.bubble.core-test
  (:require [clojure.test :refer :all]
            [bluebell.bubble.core :refer :all]))


(deftest bubble-test
  (is (bubble? (bubble 3)))
  (is (not (bubble? 3))))
