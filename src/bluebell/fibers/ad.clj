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
   :value x 
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


(defn only-non-nil [coll]
  (filter (fn [[_ x]]
            (not (nil? x))) 
          coll))

(defn map-keys [keys f sources]
  (into {} (only-non-nil 
            (map (fn [k] [k (apply f (map (fn [src] (get src k))
                                          sources))])
                 keys))))

(defn merge-derivatives [[l m r] a b]
  (let [da (:derivatives a)
        db (:derivatives b)
        ka- (set (keys da))
        kb- (set (keys db))
        kab (clojure.set/intersection ka- kb-)
        ka (clojure.set/difference ka- kab)
        kb (clojure.set/difference kb- kab)]
    (merge
     (map-keys ka l [da])
     (map-keys kb r [db])
     (map-keys kab m [da db]))))

(defn ad-add [a b]
  (ad (ops/+ (:value a) (:value b))
      (merge-derivatives [identity ops/+ identity] a b)))

(defn ad-mul [a b]
  (ad (ops/* (:value a) (:value b))
      (merge-derivatives [(partial ops/* (:value b))
                          (fn [x y]
                            (ops/+ (ops/* (:value a) y)
                                   (ops/* (:value b) x)))
                          (partial ops/* (:value a))]
                         a b)))

(defn binary-op [base-op ad-f] 
  (fn [a b]
    (if (no-ad? [a b])
      (base-op a b)
      (ad-f (to-ad a) (to-ad b)))))

(defn binary-reduce [base-op ad-f x]
  (reduce (binary-op base-op ad-f) x))

(defspecfun ops/+ 
  (::args-with-ad [x] (binary-reduce ops/+ ad-add x)))

(defspecfun ops/*
  (::args-with-ad [x] (binary-reduce ops/* ad-mul x)))

;;; TODO other ops too.
