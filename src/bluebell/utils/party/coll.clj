(ns bluebell.utils.party.coll
  (:require [bluebell.utils.party :refer :all]
            [bluebell.utils.core :as utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  More common accessors
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access the values of a map. It has to be a map!

(defn make-empty [x]
  (if (vector? x)
    [] (empty x)))

(defn reverse-if-seq [[state x]]
  [state (if (seq? x) (reverse x) x)])

;; Bug in Clojure?


(defn sort-pairs-if-possible [pairs]
  ;(println "   Sort pairs")
  (if (every? utils/comparable? (map first pairs))
    (do
      ;(println "Keys are" (map first pairs))
      (try
        (sort-by first pairs)
        (catch Throwable e
          (println "Warning: Failed to sort pairs, maybe both comparables but different types.")
          pairs)))
    
    (do
      ;(println "CANNOT BE SORTED" (map first pairs))
      ;(throw (ex-info "This is bad" {}))
      pairs)))

(defn normalize-coll [coll]
  (cond
    (map? coll) (vec (apply concat (sort-pairs-if-possible (vec coll))))
    (set? coll) (utils/sort-if-possible (vec coll))
    :default (vec coll)))

(defn make-map [proto coll]
  (first
   (reduce (fn [[m state] x]
             (if (empty? state)
               [m [x]]
               [(conj m (conj state x)) []]))
           [(make-empty proto) []] coll)))

(defn make-seq [proto coll]
  (reverse (into (make-empty proto) coll)))

(defn denormalize-coll [proto coll]
  (cond
    (map? proto) (make-map proto coll)
    (seq? proto) (make-seq proto coll)
    :default (into (make-empty proto) coll)))


(def map-vals-accessor
  (wrap-accessor
   {:desc "map-vals-accessor"
    :getter (fn [x]
              (assert (map? x))
              (vec (map (partial get x) (utils/sorted-keys x))))
    :setter (fn [x y]
              (assert (map? x))
              (assert (= (count x)
                         (count y)))
              (into x (map vector (utils/sorted-keys x) (vec y))))})  )

(def normalized-coll-accessor
  (wrap-accessor
   {:desc "Normalized coll accessor"
    :getter (fn [x] (if (coll? x)
                      (normalize-coll x)
                      []))
    :setter (fn[x y] (if (coll? x)
                       (denormalize-coll x y)
                       x))}))

;; To check that the accessor is effective
(def null-accessor
  (wrap-accessor
   {:desc "Null accessor"
    :getter (fn [x] [])
    :setter (fn [x y] x)}))

(def coll-accessor
  (wrap-accessor
   {:desc "coll-accessor"
    :getter (fn [x] (if (coll? x) x []))
    :setter (fn [x new-val] (if (coll? x) new-val x))}))


(defn access-coll-as-vec
  ([] {:desc "access-coll-as-vec"})
  ([x] (normalize-coll x))
  ([x new-value] (denormalize-coll x new-value)))

