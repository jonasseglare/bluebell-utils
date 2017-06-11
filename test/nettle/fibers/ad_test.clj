(ns nettle.fibers.ad-test
  (:require [nettle.fibers.ad :refer :all]
            [nettle.fibers.types :as types]
            [clojure.test :refer :all]))

(deftest ad-test
  (is (ad? (ad (types/primitive :double 3))))
  (is (ad? (ad (types/primitive :double 3)
               {:x (types/primitive :double 1)}))))
