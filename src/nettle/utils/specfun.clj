(ns nettle.utils.specfun
  (:require [clojure.spec :as spec]
            [nettle.utils.core :as utils]))

(def funs (atom {}))

(spec/def ::name symbol?)

(spec/def ::spec (constantly true))
(spec/def ::argsym symbol?)
(spec/def ::bodyform (constantly true))

(spec/def ::def (spec/cat :spec ::spec
                          :argsym ::argsym
                          :body (spec/* ::bodyform)))

(spec/def ::defs (spec/* (spec/spec ::def)))

(spec/def ::defspecfun (spec/cat :name ::name
                                 :defs ::defs))

(defn defs-to-map [defs]
  (into {} (map (fn [m]
                  {(:spec m) `(fn [~(:argsym m)] ~@(:body m))})
                defs)))

(defn defspecfun-sub [x]
  `(swap! funs (fn [tgt#] (update-in tgt# [(quote ~(:name x))]
              (fn [current#]
                (merge (or current# {})
                       ~(defs-to-map (:defs x))))))))
                      

(defmacro defspecfun [& args]
  (let [x (spec/conform ::defspecfun args)]
    (if (= x ::spec/invalid)
      (utils/common-error (spec/explain-str ::defspecfun args))
      (defspecfun-sub x))))
