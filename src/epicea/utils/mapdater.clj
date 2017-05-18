(ns epicea.utils.mapdater
  (:require [clojure.spec :as spec]
            [epicea.utils.defmultiple :refer [defmultiple]]))

(spec/def ::body (spec/* (constantly true)))
(spec/def ::arg (spec/cat 
                 :symbol symbol?
                 :key (spec/? (spec/cat :tag (partial = :as)
                                        :key (constantly true)))))

(defn arg-sym [x]
  (:symbol x))

(defn arg-key [x]
  (if (contains? x :key)
    (-> x :key :key)
    (keyword (:symbol x))))

(spec/def ::arglist (spec/spec (spec/* ::arg)))
(spec/def ::mapdater (spec/cat
                      :output (spec/? symbol?)
                      :arglist ::arglist
                      :body ::body))

(spec/def ::expr (spec/spec (spec/cat 
                             :op (constantly true)
                             :args (spec/* (spec/or :key keyword?
                                                    :value (constantly true))))))
(spec/def ::mapdate (spec/cat
                     :output symbol?
                     :expr ::expr))

(defn force-conform [sp x]
  (let [y (spec/conform sp x)]
    (if (= ::spec/invalid y)
       (throw (RuntimeException. (spec/explain-str sp x)))
       y)))
       

(defmacro mapdate [& args]
  (let [m (gensym)
        parsed (force-conform ::mapdate args)
        expr (:expr parsed)]
    `(fn [~m]
       (assoc ~m ~(keyword (:output parsed))
              (~(:op expr) ~@(map (fn [s] 
                                    (if (= :key (first s))
                                      `(~(second s) ~m)
                                      (second s)))
                                  (:args expr)))))))
    
    

(defn compile-mapdater [c]
  (let [mapsym (gensym)
        result-expr `(let ~(vec (reduce into [] 
                                        (map (fn [arg]
                                               [(arg-sym arg) `(get ~mapsym ~(arg-key arg))])
                                             (:arglist c))))
                       ~@(:body c))]
    `(fn [~mapsym]
       ~(if (:output c)
          `(assoc ~mapsym
                  ~(keyword (:output c))
                  ~result-expr)
          `(merge ~mapsym ~result-expr)))))
              


(defmacro mapdater [& args]
  (let [parsed (spec/conform ::mapdater args)]
    (if (= parsed ::spec/invalid)
      (throw (RuntimeException. 
              (str "Invalid mapdater syntax: "
                   (spec/explain-str ::mapdater args))))
      (compile-mapdater parsed))))

(defmacro map-of [& args]
  (assert (every? symbol? args))
  (zipmap (map keyword args)
          args))
