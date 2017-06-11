(ns nettle.fibers.ad
  (:require [clojure.spec :as spec]
            [nettle.fibers.ops :as ops]
            [nettle.fibers.types :as types]
            [nettle.fibers.core :as core]
            [nettle.utils.specfun :refer [defspecfun]]
            [nettle.utils.defmultiple :refer [defmultiple]]))

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

