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

(spec/def ::methods (spec/* (spec/spec ::method)))

(spec/def ::defmultiple (spec/cat 
                         :name ::name
                         :dispatch-fun ::dispatch-fun
                         :default (spec/? ::default)
                         :methods ::methods))

(spec/def ::defmultiple-extra
  (spec/cat 
   :name ::name
   :methods ::methods))

(defn make-method [entry]
  {(:dispatch-value entry) 
   `(fn ~@(:function entry))})

(defn make-method-map [methods]
  (reduce merge (map make-method methods)))

(def extra-methods (atom {}))
  
(defn eval-multi [name dispatch-value method-map default-key args]
  (if (contains? method-map dispatch-value) 
    (apply (get method-map dispatch-value) args)
    (let [extra (get (deref extra-methods) name)]
      (cond
        (contains? extra dispatch-value) (apply (get extra dispatch-value) args)
        (contains? method-map default-key) (apply (get method-map default-key) args)
        :default 
        (throw (RuntimeException. 
                (str "No method for dispatch value '" dispatch-value "'. "
                     "Methods are " (keys method-map))))))))

(defn defmultiple-sub [x]
  `(do
     (declare ~(:name x))
     (let [dispatch-fun# ~(:dispatch-fun x)
           method-map# ~(make-method-map (:methods x))
           default-key# ~(if (contains? x :default)
                           (-> x :default :value)
                           :default)]
       (defn ~(:name x) [& args#]
         (eval-multi (quote ~(:name x))
                     (apply dispatch-fun# args#)
                     method-map#
                     default-key#
                     args#)))))

(defmacro defmultiple [& args]
  (let [parsed (spec/conform ::defmultiple args)]
    (if (= parsed ::spec/invalid)
      (throw (RuntimeException. 
              (with-out-str 
                (spec/explain ::defmultiple args))))
      (defmultiple-sub parsed))))

(defn add-extra-methods [name methods-to-add]
  (swap! 
   extra-methods
   (fn [extra-method-map]
     (update-in
      extra-method-map [name]
      (fn [methods]
        (reduce
         merge
         methods
         methods-to-add))))))

(defn defmultiple-extra-sub [parsed]
  `(add-extra-methods
    (quote ~(:name parsed))
    ~(vec (map make-method 
               (:methods parsed)))))



(defmacro defmultiple-extra [& args]
  (let [parsed (spec/conform ::defmultiple-extra args)]
    (if (= parsed ::spec/invalid)
      (throw (RuntimeException. 
              (str "Failed to parse defmultiple-extra: "
                   (spec/explain-str ::defmultiple-extra args))))
      (defmultiple-extra-sub parsed))))
