(ns bluebell.utils-test
  (:require [clojure.test :refer :all]
            [bluebell.utils :refer :all]))

(defn test-int-op [op & pairs]
  (doseq [[src dst] pairs]
    (is (= dst (op src)))))

(deftest int-ops
  (test-int-op greater
               [2 3]
               [2.5 3]
               [-3 -2]
               [-2.5 -2])
  (test-int-op greater-or-equal
               [2 2]
               [2.5 3]
               [-3 -3]
               [-2.5 -2])
  (test-int-op less
               [2 1]
               [2.5 2]
               [-3 -4]
               [-2.5 -3])
  (test-int-op less-or-equal
               [2 2]
               [2.5 2]
               [-3 -3]
               [-2.5 -3]))

(deftest maximize-test
  (is (= 12 (maximize (range 30) #(< % 13))))
  (is (nil? (maximize (range 30) (constantly false)))))
