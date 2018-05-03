(ns bluebell.utils.setdispatch-test
  (:require [bluebell.utils.symset :as ss]
            [clojure.test :refer :all])
  (:require [bluebell.utils.setdispatch :refer :all] :reload)
  (:refer-clojure :exclude [complement]))

(def-system ts)

(defn get-feature [x]
  (cond
    (double? x) :double
    (float? x) :float
    (number? x) :number
    (keyword? x) :keyword
    (string? x) :string
    (map? x) :map
    (set? x) :set
    (coll? x) :coll))

(add ts :double)
(add ts :float)
(add ts :number)
(add ts :keyword)
(add ts :string)
(add ts :map)
(add ts :set)
(add ts :coll)
(subset-of ts :double :number)
(subset-of ts :float :number)
(subset-of ts :map :coll)
(subset-of ts :set :coll)
(subset-of ts :number :atom)
(subset-of ts :keyword :atom)
(subset-of ts :string :atom)
(subset-of ts :coll :all)
(subset-of ts :atom :all)

(def-dispatch my-plus ts get-feature)

(def-set-method my-plus [[:double a]
                         [:double b]]
  [:double-sum (+ a b)])

(def-set-method my-plus [[:float a]
                         [:float b]]
  [:float-sum (+ a b)])

(def-set-method my-plus [[:number a]
                         [:number b]]
  [:sum (+ a b)])

(def-set-method my-plus [[:coll a]
                         [:coll b]]
  (into a b))

(def-set-method my-plus [[:map a]
                         [:map b]]
  (merge a b))


(deftest test-it
  (is (= [:double-sum 7.9]
         (my-plus 3.4 4.5)))
  (is (= [:float-sum 4.5]
         (my-plus (float 2.25)
                  (float 2.25))))
  (is (= [:sum 4]
         (my-plus 1 3)))
  (is (thrown?
       Throwable
       (my-plus :a :b)))
  (is (= [:a :b :c]
         (my-plus [:a :b]
                  #{:c})))
  (is (= {:a 3 :b 4}
         (my-plus {:a 3}
                  {:b 4}))))
