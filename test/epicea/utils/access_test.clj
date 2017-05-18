(ns epicea.utils.access-test
  (:require [clojure.test :refer :all]
            [epicea.utils.access :refer :all]
            [epicea.utils.optional :refer [optional]]))

(def k (map-accessor :k {}))

(deftest basic-map-access
  (is (= 3 ((:get k) {:k 3})))
  (is (= {:k 9} ((:set k) {} 9)))
  (is ((:has? k) {:k 3}))
  (is (not ((:has? k) {:p 3}))))

(deftest base-validator
  (is (thrown? Throwable ((:validate-base k) [])))
  (is (= {:r 4} ((:validate-base k) {:r 4}))))

(deftest validate-has-test
  (is (thrown? Throwable ((:validate-has k) {:r 9})))
  (is (= {:k 111} ((:validate-has k) {:k 111}))))
