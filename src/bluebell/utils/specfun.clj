(ns bluebell.utils.specfun
  (:require [clojure.spec :as spec]
            [bluebell.utils.core :as utils]))

(def funs (atom {}))

(defn reset 
  ([] (reset! funs {}))
  ([key] (swap! funs #(assoc % key {}))))

(spec/def ::name symbol?)

(spec/def ::spec (constantly true))
(spec/def ::argsym symbol?)
(spec/def ::arglist1 (spec/spec (spec/cat :arg ::argsym)))
(spec/def ::bodyform (constantly true))

(spec/def ::def (spec/cat :spec ::spec
                          :args ::arglist1
                          :body (spec/* ::bodyform)))

(spec/def ::defs (spec/* (spec/spec ::def)))

(spec/def ::defspecfun (spec/cat :name ::name
                                 :defs ::defs))

(defn defs-to-map [defs]
  (into {} (map (fn [m]
                  {(:spec m) 
                   `(fn [~(-> m :args :arg)] 
                      ~@(:body m))})
                defs)))

(defn defspecfun-sub [x]
  `(swap! funs (fn [tgt#] (update-in tgt# [(quote ~(:name x))]
              (fn [current#]
                (merge (or current# {})
                       ~(defs-to-map (:defs x))))))))

(defn find-confs [key args]
  (filter
   (complement nil?)
   (map
    (fn [[sp impl]]
      (let [y (spec/conform sp args)]
        (if (not= y ::spec/invalid)
          [sp y impl])))
    (get (deref funs) key))))
   

(defn evaluate-specfun [key]
  (fn [& args]
    (let [confs (find-confs key args)]
      (cond
        (empty? confs) (utils/common-error "No implementation found for " key " and " args)
        (= 1 (count confs)) (let [[_ parsed-args impl] (first confs)]
                              (impl parsed-args))
        :default (utils/common-error "Conformance ambiguity for " 
                               key " and " args ": " 
                               (with-out-str
                                 (clojure.pprint/pprint 
                                  (map first confs))))))))

(defmacro defspecfun [& args]
  (let [x (spec/conform ::defspecfun args)]
    (if (= x ::spec/invalid)
      (utils/common-error (spec/explain-str ::defspecfun args))
      `(do
         ~(defspecfun-sub x)
         (def ~(:name x) (evaluate-specfun (quote ~(:name x))))))))
