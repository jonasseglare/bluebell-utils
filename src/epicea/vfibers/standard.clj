(ns epicea.vfibers.standard
  (:require [epicea.vfibers.core :refer :all]))

(def primitive-list [[[:bool] {}]
                     [[:float :float32] {}]
                     [[:float64 :double] {}]
                     [[:int32 :int] {}]
                     [[:int64 :long] {}]])

(defn add-primitive-entry [dst [names properties]]
  (reduce
   (fn [dst name]
     (assoc dst name properties))
   dst
   names))

(def primitives (reduce add-primitive-entry {} primitive-list))

(defn primitive-type? [x]
  (contains? primitives x))

(defn primitive
  ([t] (assert (primitive-type? t))
   (primitive-expr t))
  ([t x] 
   (assert (primitive-type? t))
   (primitive-expr t x)))
