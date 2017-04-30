(ns epicea.utils.defmultiple
  (:require [clojure.spec :as spec]))


(spec/def ::name symbol?)
(spec/def ::dispatch-fun (constantly true))
(spec/def ::dispatch-value (constantly true))
(spec/def ::whatever (constantly true))

(defn defmultiple [& args]
  (println "GOT ARGS: " args))

(spec/def ::defmultiple #(= 'defmultiple %))

(spec/def ::method (spec/cat
                    :dispatch-value ::dispatch-value
                    :function (spec/* ::whatever)))

(spec/def ::defmultiple (spec/cat 
                         :name ::name
                         :dispatch-fun ::dispatch-fun
                         :methods (spec/* (spec/spec ::method)))) ;; explicit call to spec/spec.
