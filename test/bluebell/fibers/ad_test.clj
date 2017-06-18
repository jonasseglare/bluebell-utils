(ns bluebell.fibers.ad-test
  (:require [bluebell.fibers.ad :refer :all :as ad]
            [bluebell.fibers.types :as types]
            [bluebell.fibers.core :as core]
            [bluebell.fibers.ops :as ops]
            [clojure.test :refer :all]
            [clojure.spec :as spec]))

(deftest ad-test
  (is (ad? (ad (types/primitive :double 3))))
  (is (ad? (ad (types/primitive :double 3)
               {:x (types/primitive :double 1)}))))

(deftest args-test
  (is (spec/valid?
       ::ad/args-with-ad
       [(types/input 1) 
        (types/input 2)
        (types/input 3)
        (ad (types/primitive :double 3))]))
  (is (not (spec/valid?
            ::ad/args-with-ad
            [1 2 3]))))

(deftest add-test
  (is (= 10 (:x (core/expand (ops/+ (ad (types/input 3)) (ad (types/input 4)) (types/input 3)))))))
