(ns bluebell.fibers.ad
  (:require [clojure.spec :as spec]
            [bluebell.fibers.ops :as ops]
            [bluebell.fibers.types :as types]
            [bluebell.fibers.core :as core]
            [bluebell.utils.specfun :refer [defspecfun]]
            [bluebell.utils.defmultiple :refer [defmultiple]]))

(spec/def ::scalar-map (spec/map-of (constantly true) types/scalar?))

(defn ad-sub [x derivative-map]
  (assert (types/scalar? x))
  (assert (spec/valid? ::scalar-map derivative-map))
  {:type :ad 
   :x x 
   :derivatives derivative-map
   :scalar? true})

(defn ad 
  ([x] (ad x {}))
  ([x derivative-map] (ad-sub x derivative-map)))

(defn is-type? 
  ([x t]
   (if (map? x)
     (= t (:type x))))
  ([t] #(is-type? % t)))

(def ad? (is-type? :ad))

(spec/def ::ad ad?)
(spec/def ::args-with-ad (spec/and 
                                 ::types/scalars
                                 #(some ad? %)))

(defn no-ad? [args]
  (not (some ad? args)))

(defn to-ad [x]
  (if (ad? x) x (ad x)))

(defn ad-add [a b]
  (ad (ops/+ (:x a) (:x b))))


(defn binary-op [base-op ad-f] 
  (fn [a b]
    (if (no-ad? [a b])
      (base-op a b)
      (ad-f (to-ad a) (to-ad b)))))

(defn binary-reduce [base-op ad-f x]
  (reduce (binary-op base-op ad-f) x))

(defspecfun ops/+ 
  (::args-with-ad [x] (binary-reduce ops/+ ad-add x)))
