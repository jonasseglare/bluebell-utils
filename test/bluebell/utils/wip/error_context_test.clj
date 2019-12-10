(ns bluebell.utils.wip.error-context-test
  (:require [bluebell.utils.wip.error-context :refer :all]
            [clojure.test :refer :all])
  (:refer-clojure :exclude [do]))

(deftest error-value-test
  (is (not (error? 119)))
  (let [x (error "Mjao" {:a 119})]
    (is (error? x))
    (is (= "Mjao" (message x)))
    (is (= {:a 119} (data x)))))

(deftest context-test
  (is (not (context? (error "MJao"))))
  (is (context? (context))))
