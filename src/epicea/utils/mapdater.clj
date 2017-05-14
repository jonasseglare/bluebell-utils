(ns epicea.utils.mapdater
  (:require [clojure.spec :as spec]))

(spec/def ::body (spec/* (constantly true)))
(spec/def ::arglist (spec/coll-of symbol?))
(spec/def ::mapdater (spec/cat
                      :output symbol?
                      :arglist ::arglist
                      :body ::body))


(defn compile-mapdater [c]
  (let [mapsym (gensym)]
    `(fn [~mapsym]
       (assoc ~mapsym
              ~(keyword (:output c))
              (let ~(vec (reduce into [] 
                                 (map (fn [keysym]
                                        [keysym `(~(keyword keysym) ~mapsym)])
                                      (:arglist c))))
                ~@(:body c))))))


(defmacro mapdater [& args]
  (let [parsed (spec/conform ::mapdater args)]
    (if (= parsed ::spec/invalid)
      (throw (RuntimeException. 
              (str "Invalid mapdater syntax: "
                   (spec/explain-str ::mapdater args))))
      (compile-mapdater parsed))))
