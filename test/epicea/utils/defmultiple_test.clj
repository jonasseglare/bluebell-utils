(ns epicea.utils.defmultiple-test
  (:require [clojure.test :refer :all]
            [epicea.utils.defmultiple :refer :all :as d]
            [clojure.spec :as spec]))

(deftest defmultiple-test
  (is (= {:name 'evaluate
          :dispatch-fun :op
          :methods [{:dispatch-value :katt, :function [['x] '(+ x 3)]}]}
         (spec/conform ::d/defmultiple '(evaluate :op (:katt [x] (+ x 3)))))))
