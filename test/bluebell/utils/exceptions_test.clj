(ns bluebell.utils.exceptions-test
  (:require [bluebell.utils.exceptions :refer :all :as ex]
            [clojure.test :refer :all]))

(deftest exceptions-test
  (is (= :kattskit
         (try
           (tag :kattskit (assert false))
           (catch Throwable e
             (-> e
                 ex-data
                 ::ex/tag)))))

  
  (is (= ["It failed: " :a]
         (either a (tag :a (assert false))
                 b ["It failed: " (::ex/tag a)])))
  
  (let [[x y] (either a (either a (tag :a (assert false))
                                b (tag :b (assert false)))
                      b [:what-we-got a])]
    (is (= :what-we-got x))
    (is (= #{:a :b}
           (set (keys y)))))

  (is (= :asdf (-> (either a (tag :not-a-number (expect number? :asdf))
                           _ a)
                   ::ex/value
                   ::ex/value))))


