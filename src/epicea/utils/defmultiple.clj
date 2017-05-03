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

(spec/def ::default (spec/cat
                     :default-key #(= % :default)
                     :value (constantly true)))

(spec/def ::defmultiple (spec/cat 
                         :name ::name
                         :dispatch-fun ::dispatch-fun
                         :default (spec/? ::default)
                         :methods (spec/* (spec/spec ::method)))) ;; explicit call to spec/spec.

(defn make-method [entry]
  {(:dispatch-value entry) 
   (eval `(fn ~@(:function entry)))})

(defn make-method-map [methods]
  (reduce merge (map make-method methods)))

(defn eval-multi [dispatch-value method-map default-key args]
  (cond
    (contains? method-map dispatch-value) (apply (get method-map dispatch-value) args)
    (contains? method-map default-key) (apply (get method-map default-key) args)
    :default (throw (RuntimeException. (str "No method for dispatch value '" dispatch-value "'. "
                                            "Methods are " (keys method-map))))))
                                                 

(defn defmultiple-sub [x]
  `(let [dispatch-fun# ~(eval (:dispatch-fun x))
         method-map# ~(make-method-map (:methods x))
         default-key# ~(eval (-> x :default :value))]
     (defn ~(:name x) [& args#]
       (eval-multi (apply dispatch-fun# args#)
                   method-map#
                   default-key#
                   args#))))

(defmacro defmultiple [& args]
  (let [parsed (spec/conform ::defmultiple args)]
    (if (= parsed ::spec/invalid)
      (throw (RuntimeException. 
              (with-out-str 
                (spec/explain ::defmultiple args))))
      (defmultiple-sub parsed))))
