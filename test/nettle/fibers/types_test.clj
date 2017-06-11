(ns nettle.fibers.types-test
  (:require [nettle.fibers.types :refer :all]
            [nettle.fibers.core :as fibers]
            [clojure.test :refer :all]))

(deftest primitive-test
  (is (fibers/node? (primitive :double 9)))
  (is (fibers/node? (primitive :double)))
  (is (scalar? (primitive :double))))

(deftest common-datatype-test
  (= :double 
     (common-datatype [(primitive :double 3) (primitive :float 9)])))
