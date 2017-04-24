(ns epicea.poly.core
  (:require [clojure.spec :as spec]
            [epicea.utils.access :as access]))

(declare multi-fn)

(def empty-method-spec {:default nil :list []})

(def default-field (access/map-accessor :default empty-method-spec))
(def list-field (access/map-accessor :list empty-method-spec))
(def set-default (access/setter default-field))
(def update-list (access/updater list-field))
(defn add-method [method-spec method]
  (update-list method-spec #(conj % method)))

(def ^:private method-map (atom {}))

(defmacro declpoly [name default-impl]
  (swap! method-map #(assoc % name (set-default {} (eval default-impl))))
  nil)
