(ns epicea.utils.access-test
  (:require [clojure.test :refer :all]
            [epicea.utils.access :refer :all]
            [epicea.utils.optional :refer [optional]]))

(def k (key-accessor :k {:valid-value? int? :default-value 0}))

(deftest basic-map-access
  (is (= 3 ((:get k) {:k 3})))
  (is (= {:k 9} ((:set k) {} 9)))
  (is ((:can-get? k) {:k 3}))
  (is (not ((:can-get? k) {:p 3}))))

(deftest base-validator
  (is (thrown? Throwable ((:validate-base k) [])))
  (is (= {:r 4} ((:validate-base k) {:r 4}))))

(deftest validate-has-test
  (is (thrown? Throwable ((:validate-has k) {:r 9})))
  (is (= {:k 111} ((:validate-has k) {:k 111}))))

(deftest validate-value-test
  (is (thrown? Throwable ((:validate-value k) "asfdasdf")))
  (is ((:validate-value k) 9)))

(deftest get-optional-test
  (is (= [3] ((:get-optional k) {:k 3})))
  (is (nil? ((:get-optional k) {:r 3})))
  (is (thrown? Throwable ((:get-optional k) [])))
  (is (thrown? Throwable ((:get-optional k) {:k :a}))))

(deftest checked-get-test
  (is (= 3 ((:checked-get k) {:k 3})))
  (is (thrown? Throwable ((:checked-get k) {:k :a}))))

(deftest checked-set-test
  (is (= {:k 9} ((:checked-set k) {:k 3} 9))))

(deftest update-test
  (is (= {:k 10} ((:update k) {:k 9} inc))))

(deftest prepare-test
  (is (= {:k 0} ((:prepare k) {})))
  (is (= {:k 9} ((:prepare k) {:k 9}))))

(deftest get-or-default-test
  (is (= 9 ((:get-or-default k) {:k 9})))
  (is (= 0 ((:get-or-default k) {}))))

;;;;;;;;;;;;;;;;;;;;;;;
(def v (index-accessor 1))

(deftest vector-tests
  (is (= 119 ((:checked-get v) [3 119])))
  (is (= [nil 9] ((:checked-set v) [nil nil] 9)))
  (is (not ((:can-get? v) []))))

(def a (key-accessor :a))
(def b (key-accessor :b))
(def c (key-accessor :c))

(defn abc-add [x]
  ((:checked-set c)
   x (+ ((:checked-get a) x)
        ((:checked-get b) x))))

(deftest abc-test
  (is (= {:a 3 :b 4 :c 7} (abc-add {:a 3 :b 4}))))

(deftest remove-test
  (is (= {:a 3} ((:checked-remove b) {:a 3 :b 4}))))
