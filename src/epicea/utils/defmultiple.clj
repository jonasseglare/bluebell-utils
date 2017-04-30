(ns epicea.utils.defmultiple
  (:require [clojure.spec :as spec]))


(spec/def ::name symbol?)
(spec/def ::dispatch-fun (constantly true))
(spec/def ::dispatch-value (constantly true))
(spec/def ::whatever (constantly true))


(spec/def ::defmultiple #(= 'defmultiple %))

(spec/def ::method (spec/cat
                    :dispatch-value ::dispatch-value
                    :function (spec/* ::whatever)))

(spec/def ::defmultiple (spec/cat 
                         :name ::name
                         :dispatch-fun ::dispatch-fun
                         :methods (spec/* (spec/spec ::method)))) ;; explicit call to spec/spec.

(defn make-method [entry]
  {(:dispatch-value entry) 
   (eval `(fn ~@(:function entry)))})

(defn make-method-map [methods]
  (reduce merge (map make-method methods)))

(defn defmultiple-sub [x]
  `(let [dispatch-fun# ~(eval (:dispatch-fun x))
         method-map# ~(make-method-map (:methods x))]
     (defn ~(:name x) [& args#]
       (let [dv# (apply dispatch-fun# args#)
             m# (get method-map# dv#)]
         (if (nil? m#) 
           (throw (RuntimeException. (str "No dispatch value for '" dv# "'")))
           (apply m# args#))))))

(defmacro defmultiple [& args]
  (let [parsed (spec/conform ::defmultiple args)]
    (if (= parsed ::spec/invalid)
      (throw (RuntimeException. 
              (with-out-str 
                (spec/explain ::defmultiple args))))
      (defmultiple-sub parsed))))
