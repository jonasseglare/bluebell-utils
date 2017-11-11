(ns bluebell.bubble.core-test
  (:require [clojure.test :refer :all]
            [bluebell.bubble.core :refer :all]))

(def my-add (protect-fn +))
(def my-mul (protect-fn *))
(defn my-sqr [x] (my-mul x x))
(def my-sqrt (protect-fn #(Math/sqrt %)))

(defn pythagoras [a b]
  (my-sqrt (my-add (my-sqr a) (my-sqr b))))

(deftest bubble-test
  (is (bubble? (bubble 3)))
  (is (not (bubble? 3)))
  (is (= 7 (my-add 3 4)))
  (is (= (bubble 3) (my-add (bubble 3) 4)))
  (is (= 5 (alts (bubble 3) 5 (bubble 4))))
  (is (= (bubble 3) (anti 3)))
  (is (= 3 (anti (bubble 3))))
  (is (contains-bubble? [ 1 2 [(bubble 3)]]))
  (is (not (contains-bubble? [1 2 [3]])))
  (is (= [1 2 3] (bubble-up [1 2 3])))
  (is (= (bubble [1 2 (bubble 3)])
         (bubble-up [1 2 (bubble 3)])))
  (is (= (bubble [1 2 3])
         (bubble-up (bubble [1 2 3]))))
  (is (= (bubble false)
         (protect-if (bubble false) 3 4)))
  (is (= [1] (filter-first number? :a :b 1 :c)))
  (is (nil? (filter-first number? :a :b :c)))
  (is (= 3 (protect-if true 3 4)))
  (is (= 4 (protect-if false 3 4)))
  (is (= nil (protect-if false 3)))
  (is (= 5.0 (pythagoras 3 4)))
  (is (= (bubble 3) (pythagoras (bubble 3) 4))))
