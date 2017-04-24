(ns epicea.poly.core
  (:require [clojure.spec :as spec]
            [epicea.utils.access :as access]))

(declare multi-fn)

(def empty-method-list [])

(defn add-method [obj x]
  (conj obj x))

(def ^:private method-map (atom {}))

(defn polyfun [name default-impl]
  (fn [& args]
    (loop [impls (get (deref method-map) name)]
      (if (empty? impls)
        (apply default-impl args)
        (if-let [[result] (apply (first impls) args)]
          result
          (recur (rest impls)))))))

(defmacro declpoly [name default-impl]
  (swap! method-map #(assoc % name empty-method-list))
  `(def ~name ~(polyfun name (eval default-impl))))

(defn make-method [arglist body-forms]
  (eval `(fn [~@arglist] [(do ~@body-forms)])))

(defn register-poly [m name arglist body-forms]
  (update-in m [name] #(add-method % (make-method arglist body-forms))))

(defmacro defpoly [name arglist & body-forms]
  (swap! method-map #(register-poly % name arglist body-forms)))
