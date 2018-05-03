(ns bluebell.utils.symset-test
  (:require [clojure.test :refer :all]
            [bluebell.utils.specutils :as sutils]
            [bluebell.utils.symset :refer :all :as ss]))

(deftest basic-tests
  (is (set-registry? empty-set-registry))
  (let [with-x (sutils/force-conform
                ::ss/registry
                (belongs-to empty-set-registry :x :numbers))
        _ (is (= #{:numbers} (set-memberships with-x :x)))
        _ (is (empty? (set-memberships with-x :y)))
        ]
    (println with-x)))
