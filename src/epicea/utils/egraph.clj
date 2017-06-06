(ns epicea.utils.egraph
  (:require [clojure.spec :as spec]
            [epicea.utils.access :as access]))


(spec/def ::unique-tag (partial = ::node))

(spec/def ::node (spec/keys :req-un [::unique-tag]))

(defn node? [x]
  (spec/valid? ::node x))

(def empty-node {:unique-tag ::node})
(def settings {:default-base empty-node})

(def -simple? (access/key-accessor :simple? (merge settings {:valid-value? boolean?})))
(def -type (access/key-accessor :type (merge settings {:valid-value? keyword?})))
(def -expr (access/key-accessor :expr settings))
(def -args (access/key-accessor :args (merge settings {:valid-value? vector?})))
(def -make (access/key-accessor :make (merge settings {:valid-value? fn?})))

(defn simple-expr? [x]
  (or (keyword? x)
      (symbol? x)
      (number? x)
      (string? x)
      (boolean? x)))
      

(defn primitive-expr [tp value]
  (access/build -type tp
                -expr value
                -simple? (simple-expr? value)))
