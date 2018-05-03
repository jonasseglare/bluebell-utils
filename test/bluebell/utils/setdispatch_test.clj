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

(def-set-method my-plus "Double addition"
  [[:double a]
   [:double b]]
  [:double-sum (+ a b)])

(def-set-method my-plus "Float addition"
  [[:float a]
   [:float b]]
  [:float-sum (+ a b)])

(def-set-method my-plus "General addition"
  [[:number a]
   [:number b]]
  [:sum (+ a b)])

(def-set-method my-plus "Concatenate collections"
  [[:coll a]
   [:coll b]]
  (into a b))

(def-set-method my-plus "Merge maps"
  [[:map a]
   [:map b]]
  (merge a b))

(def-set-method my-plus
  "Returns a data structure representing string concatenation"
  [[:string a]
   [:string b]]
  
  [:str-cat a b])

(def-set-method my-plus
  "Actually concatenates the strings"
  [[:string a]
   [:string b]]
  (str a b))

(defn coarse-feature [x]
  (cond
    (number? x) :number
    (coll? x) :coll
    :default :atom))

(def-set-method my-plus [[coarse-feature :number a]
                         [coarse-feature :number b]
                         [coarse-feature :number c]]
  [:triple-plus (+ a b c)])


(deftest test-it
  (is (= [:double-sum 7.9]
         (my-plus 3.4 4.5)))
  (is (= [:float-sum 4.5]
         (my-plus (float 2.25)
                  (float 2.25))))
  (is (= [:sum 4]
         (my-plus 1 3)))

  ;; Thrown because not implemented
  (is (thrown?
       Throwable
       (my-plus :a :b)))

  ;; Thrown because ambiguous
  (is (thrown?
       Throwable
       (my-plus "a" "b")))
  (is (= [:a :b :c]
         (my-plus [:a :b]
                  #{:c})))
  (is (= {:a 3 :b 4}
         (my-plus {:a 3}
                  {:b 4})))
  (is (= [:triple-plus 6]
         (my-plus 1 2 3)))
  (is (thrown?
       Throwable
       (my-plus 1 2 :a))))
