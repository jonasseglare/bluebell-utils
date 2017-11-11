(ns bluebell.bubble.core-test
  (:require [clojure.test :refer :all]
            [bluebell.bubble.core :refer :all]))

(def my-add (protect-fn +))

(deftest bubble-test
  (is (bubble? (bubble 3)))
  (is (not (bubble? 3)))
  (is (= 7 (my-add 3 4))))
