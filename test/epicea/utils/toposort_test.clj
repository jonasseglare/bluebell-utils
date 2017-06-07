(ns epicea.utils.toposort-test
  (:require [clojure.test :refer :all]
            [epicea.utils.toposort :refer :all]))

(deftest pred-map
  (is (= {:b [:a] :c [:a :b]}
         (make-predecessor-map test-map))))
