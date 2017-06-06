(ns epicea.utils.egraph-test
  (:require [clojure.test :refer :all]
            [epicea.utils.egraph :refer :all :as egraph]))

(deftest egraph-test-node?
  (is (node? {:unique-tag ::egraph/node :kattskit 9}))
  (is (not (node? {:unique-tag 9}))))
