(ns bluebell.utils.symset-test
  (:require [clojure.test :refer :all]
            [bluebell.utils.specutils :as sutils]
            [bluebell.utils.symset :refer :all :as ss]))

(deftest basic-tests
  (is (set-registry? empty-set-registry))
  (let [a (sutils/force-conform
                ::ss/registry
                (belongs-to empty-set-registry :x :numbers))
        _ (is (= #{:numbers} (set-memberships a :x)))
        _ (is (empty? (set-memberships a :y)))
        b (belongs-to a :x :elements)
        _ (is (= #{:numbers :elements} (set-memberships b :x)))
        _ (is (= #{:numbers :elements} (all-sets b)))
        c (subset-of b :rationals :numbers)
        _ (is (= #{:numbers :elements :rationals} (all-sets c)))
        _ (is (belongs-to? c :x :numbers))
        _ (is (not (belongs-to? c :x :rationals)))
        ]
    ))
