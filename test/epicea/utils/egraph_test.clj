(ns epicea.utils.egraph-test
  (:require [clojure.test :refer :all]
            [epicea.utils.access :as access]
            [epicea.utils.egraph :refer :all :as egraph]))

(deftest egraph-test-node?
  (is (node? {:unique-tag ::egraph/node :kattskit 9}))
  (is (node? empty-node))
  (is (not (node? {:unique-tag 9})))
  (is (node? (primitive-expr :double 9)))
  (let [a (primitive-expr :double 9)
        b (primitive-expr :double '(+ 3 4))]
    (is (access/get a -simple?))
    (is (not (access/get b -simple?)))))
