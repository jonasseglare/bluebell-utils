(ns epicea.utils-test
  (:require [clojure.test :refer :all]
            [epicea.utils :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (map-with-keys? {:rulle 119} #{:rull :rulle}))
    (is (map-with-keys? {:rulle 119} #{:rulle}))
    (is (not (map-with-keys? {:rulle 119} #{:rull})))
    (is (not (map-with-keys? [:rulle] #{:rulle})))))
