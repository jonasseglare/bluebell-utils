(ns nettle.fibers.ops
  (:refer-clojure :exclude [+ - * / inc dec])
  (:require [clojure.spec :as spec]
            [nettle.fibers.core :as core]
            [nettle.fibers.types :as types]
            [nettle.utils.access :as access]
            [nettle.utils.specfun :as specfun]
            [nettle.utils.defmultiple :refer [defmultiple-extra]]))

(spec/def ::primitive types/primitive?)
(spec/def ::primitives (spec/* ::primitive))
(spec/def ::primitive-pair (spec/and ::primitives
                                     #(= 2 (count %))))



(defn primitive-op [op]
  (fn [args]
    (access/build core/-args args
                  core/-datatype (types/common-datatype args)
                  core/-nodetype op)))

(defn construct [t argspec arg]
  (if (= t (access/get argspec core/-datatype))
    arg
    `(~(-> types/primitives t :construct) ~arg)))


(defn get-node-args [node]
  (access/get node core/-args))

(defn cast-to-common-type [t argnodes args]
  (assert (= (count argnodes)
             (count args)))
  (map (fn [argspec arg]
         (construct t argspec arg))
       argnodes
       args))
       
           

(defn type-op [t op-key]
  (-> types/primitives t op-key))
         

(defn op-primitives-code [op-key]
  (fn [node args]
    (let [t (access/get node core/-datatype)]
      `(~(type-op t op-key) ~@args))))

;;;;; Addition
(def add-primitives (primitive-op :add-op))
(def add-primitives-code (op-primitives-code :add-op))


;;;;; Subtraction
(def sub-primitives (primitive-op :sub-primitives))
(def sub-primitives-code (op-primitives-code :sub-op))

;;;; 

;;;;; Dispatch  
(defmultiple-extra make-node
  (:add-op [node args] (add-primitives-code node args))
  (:sub-op [node args] (sub-primitives-code node args)))

;(specfun/reset)

(defn remove-nils [x]
  (filter (complement nil?) x))

(specfun/defspecfun +
  (::primitives [x] (add-primitives x)))

(specfun/defspecfun -
  (::primitive-pair [x] (sub-primitives x)))


