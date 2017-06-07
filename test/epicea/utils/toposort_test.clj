(ns epicea.utils.toposort-test
  (:require [clojure.test :refer :all]
            [epicea.utils.toposort :refer :all]))

(deftest pred-map
  (is (= {:b [:a] :c [:a :b]}
         (make-predecessor-map test-map)))
  (is (= [:a :b :c] (toposort test-map)))
  (is (= [:a :b :c :d] (toposort {:a [:b :c] :b [:c] :c [:d]})))
  (is (nil? (toposort {:a [:b :c] :b [:c] :c [:d :a]}))))
