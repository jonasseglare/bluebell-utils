(ns epicea.utils.core-test
  (:require [clojure.test :refer :all]
            [epicea.utils.core :refer :all]))

(deftest map-with-keys-test
  (testing "Testing map with keys"
    (is (map-with-keys? {:rulle 119} #{:rulle}))
    (is (map-with-keys? {:rulle 119} #{:rulle}))
    (is (not (map-with-keys? {:rulle 119} #{:rull})))
    (is (not (map-with-keys? [:rulle] #{:rulle})))))

(deftest matrix-index-test
  (testing "Matrix index computation"
    (is (= 0 (compute-matrix-index [3 4] [0 0])))
    (is (= 1 (compute-matrix-index [3 4] [1 0])))
    (is (= 3 (compute-matrix-index [3 4] [0 1])))
    (is (= 4 (compute-matrix-index [3 4] [1 1])))))

(deftest provide-arguments-test
  (testing "Providing arguments"
    (let [f (provide-arguments get [[{:a 3 :b 4}] nil])]
      (is (= 3 (f :a)))
      (is (= 4 (f :b))))
    (let [f (provide-arguments get [nil [:a]])]
      (is (= 9 (f {:a 9})))
      (is (= 12 (f {:a 12}))))))

(deftest map-hierarchy-test
  (testing "Flattening..."
    (is (= (flatten-map-hierarchy {:a {1 :c 2 :d} :b {3 :k}})
           {[:a 1] :c, [:a 2] :d, [:b 3] :k}))))

(deftest group-transducer-test
  (is (reduce ((bundle 2) conj) [] [1 2 3 4 5 6])
      [[1 2] [3 4] [5 6]]))
