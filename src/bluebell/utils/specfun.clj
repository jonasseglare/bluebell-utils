(ns bluebell.utils.specfun
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.core :as utils]))

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

(spec/def ::declspecfun (spec/cat :name ::name))

(defn defs-to-map [defs]
  (into {} (map (fn [m]
                  {(:spec m) 
                   `(fn [~(-> m :args :arg)] 
                      ~@(:body m))})
                defs)))

(defn defspecfun-sub [x]
  `(swap! (~(:name x) ::map) 
          (fn [tgt#] 
            (merge (or tgt# {})
                   ~(defs-to-map (:defs x))))))

(defn find-confs [funs args]
  (filter
   (complement nil?)
   (map
    (fn [[sp impl]]
      (let [y (spec/conform sp args)]
        (if (not= y ::spec/invalid)
          [sp y impl])))
    (deref funs))))
   

(defn evaluate-specfun [key m]
  (fn [& args]
    (if (= [::map] args)
      m
      (let [confs (find-confs m args)]
        (cond
          (empty? confs) (utils/common-error "No implementation found for " key " and " args)
          (= 1 (count confs)) (let [[_ parsed-args impl] (first confs)]
                                (impl parsed-args))
          :default (utils/common-error "Conformance ambiguity for " 
                                       key " and " args ": " 
                                       (with-out-str
                                         (clojure.pprint/pprint 
                                          (map first confs)))))))))

(defmacro declspecfun [name]
  `(let [m# (atom {})]
     (def ~name (evaluate-specfun ~name m#))))

(defmacro defspecfun [& args]
  (let [x (spec/conform ::defspecfun args)]
    (if (= x ::spec/invalid)
      (utils/common-error (spec/explain-str ::defspecfun args))
      (defspecfun-sub x))))
