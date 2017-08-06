(ns bluebell.fibers.ops
  (:refer-clojure :exclude [+ - * / inc dec])
  (:require [clojure.spec.alpha :as spec]
            [bluebell.fibers.core :as core]
            [bluebell.fibers.types :as types]
            [bluebell.utils.access :as access]
            [bluebell.utils.specfun :as specfun]
            [bluebell.utils.defmultiple :refer [defmultiple-extra]]))

(spec/def ::primitive types/primitive?)
(spec/def ::primitives (spec/* ::primitive))

(defn n-primitives [n]
  (spec/and ::primitives
            #(= n (count %))))
  
(spec/def ::primitive-pair (n-primitives 2))
(spec/def ::one-primitive (n-primitives 1))

(defn primitive-op [op args]
  (let [dt (types/common-datatype args)]
    (merge
     (get types/primitives dt)
     (types/scalar
      (access/build core/-args args
                    core/-datatype dt
                    core/-nodetype op)))))

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

(defn op-primitives-code [op-key node args]
  (let [t (access/get node core/-datatype)]
    `(~(type-op t op-key) ~@args)))

(defmacro decl-primitive [op op-key op-spec]
  `(do 
     (defmultiple-extra bluebell.fibers.core/make-node
       (~op-key [node# args#] 
        (op-primitives-code ~op-key node# args#)))
     (specfun/defspecfun ~op
       (~op-spec [x#] (primitive-op ~op-key x#)))))

(specfun/declspecfun +)
(specfun/declspecfun -)
(specfun/declspecfun *)
(specfun/declspecfun inc)
(specfun/declspecfun dec)

(decl-primitive + :add-op ::primitives)
(decl-primitive - :sub-op ::primitive-pair)
(decl-primitive * :mul-op ::primitives)
(decl-primitive inc :inc-op ::one-primitive)
(decl-primitive dec :dec-op ::one-primitive)
(decl-primitive - :negate-op ::one-primitive)
