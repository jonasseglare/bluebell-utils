(ns bluebell.utils.setdispatch-test
  (:require [bluebell.utils.setdispatch :refer :all] :reload))

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
  [:sum (+ a b)])

