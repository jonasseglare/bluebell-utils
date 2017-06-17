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
  {:type :ad :x x :derivatives derivative-map})

(defn ad 
  ([x] (ad x {}))
  ([x derivative-map] (ad-sub x derivative-map)))

(defn is-type? 
  ([x t]
   (if (map? x)
     (= t (:type x))))
  ([t] #(is-type? % t)))

(def ad? (is-type? :ad))

