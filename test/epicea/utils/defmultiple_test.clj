(ns epicea.utils.defmultiple-test
  (:require [clojure.test :refer :all]
            [epicea.utils.defmultiple :refer :all :as d]
            [clojure.spec :as spec]))

(deftest defmultiple-test
  (is (= {:name 'evaluate
          :dispatch-fun :op
          :methods [{:dispatch-value :katt, :function [['x] '(+ x 3)]}]}
         (spec/conform ::d/defmultiple '(evaluate :op (:katt [x] (+ x 3)))))))

(defmultiple kattskit2 :op :default :unknown
  (:plus [k] (+ (:a k) (:b k)))
  (:minus [k] (- (:a k) (:b k)))
  (:unknown [k] "Unknown"))

(deftest kattskit2-test
  (is (= 7 (kattskit2 {:op :plus :a 3 :b 4})))
  (is (= -1 (kattskit2 {:op :minus :a 3 :b 4})))
  (is (= "Unknown" (kattskit2 {:op :asdf}))))
