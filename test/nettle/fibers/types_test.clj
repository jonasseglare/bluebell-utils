(ns nettle.fibers.types-test
  (:require [nettle.fibers.types :refer :all]
            [nettle.fibers.core :as fibers]
            [clojure.test :refer :all]))

(deftest primitive-test
  (is (fibers/node? (primitive :double 9)))
  (is (fibers/node? (primitive :double)))
  (is (scalar? (primitive :double))))

(deftest ad-test
  (is (ad? (ad (primitive :double 3))))
  (is (ad? (ad (primitive :double 3)
               {:x (primitive :double 1)}))))
