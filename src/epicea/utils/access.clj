(ns epicea.utils.access
  (:require [epicea.utils.debug :as dbg]))

(defn accessor? [x]
  (and (map? x)
       (every? #(contains? x %) [:id :default-parent :set :get :has?])))

;;;;; Standard accessors
(defn map-accessor [key]
  {:id [[::map-accessor key]]
   :default-parent {}
   :get key
   :has? #(contains? % key)
   :set (fn [obj x] (assoc obj key x))})
          
;;;;; Utilities
(defn getx [accessor obj]
  ((:get accessor) obj))

(defn setx [accessor obj x]
  ((:set accessor) obj x))

(defn updatex [accessor obj f]
  ((:set accessor) obj (f ((:get accessor) obj))))

(defn has? [accessor obj]
  ((:has? accessor) obj))

(defn getx-or-default [accessor obj]
  (if ((:has? accessor) obj)
    ((:get accessor) obj)
    (:default-parent obj)))

(defn er [which]
  (fn [accessor]
    (assert (accessor? accessor))
    (get accessor which)))

(def hasser? (er :has?))

(def getter (er :get))

(def setter (er :set))

(defn updater [accessor]
  (fn [obj f] (updatex accessor obj f)))

;;;;; Composition

(defn compose-getx [a b]
  (fn [x] (-> x a b)))

(defn compose-has? [a b]
  (fn [x] (and ((:has? a) x) 
               ((:has? b) ((:get a) x)))))

(defn compose-setx [a b]
  (fn [obj x] 
    (setx a obj (setx b (getx-or-default a obj) x))))

(defn compose [a b]
  {:id (concat (:id a) (:id b))
   :default-parent (:default-parent a)
   :get (compose-getx (:get a) (:get b))
   :has? (compose-has? a b)
   :set (compose-setx a b)})
