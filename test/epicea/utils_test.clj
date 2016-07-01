(ns epicea.utils-test
  (:require [clojure.test :refer :all]
            [epicea.utils :refer :all]))

(deftest map-with-keys-test
  (testing "Testing map with keys"
    (is (map-with-keys? {:rulle 119} #{:rull :rulle}))
    (is (map-with-keys? {:rulle 119} #{:rulle}))
    (is (not (map-with-keys? {:rulle 119} #{:rull})))
    (is (not (map-with-keys? [:rulle] #{:rulle})))))

(deftest matrix-index-test
  (testing "Matrix index computation"
    (is (= 0 (compute-matrix-index [3 4] [0 0])))
    (is (= 1 (compute-matrix-index [3 4] [1 0])))
    (is (= 3 (compute-matrix-index [3 4] [0 1])))
    (is (= 4 (compute-matrix-index [3 4] [1 1])))))
