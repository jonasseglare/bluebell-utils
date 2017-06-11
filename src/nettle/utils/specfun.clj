(ns nettle.utils.specfun
  (:require [clojure.spec :as spec]))

(def funs (atom {}))

(spec/def ::name symbol?)

(spec/def ::spec (constantly true))
(spec/def ::argsym symbol?)
(spec/def ::bodyform (constantly true))

(spec/def ::def (spec/cat :spec ::spec
                          :argsym ::argsym
                          :body (spec/* ::bodyform)))

(spec/def ::defs (spec/* (spec/spec ::def)))

(spec/def ::defs (spec/cat :name ::name
                           :defs ::defs))
