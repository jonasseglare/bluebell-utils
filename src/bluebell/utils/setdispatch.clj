(ns bluebell.utils.setdispatch
  (:require [bluebell.utils.symset :as ss]))

(defmacro def-system [system-name]
  `(def ~system-name (atom ss/empty-set-registry)))


#_(defmacro def-dispatch [fn-name system-name]
    `(let [dispatch-map# ]) (defn ~fn-name [& args]))

#_ (defmacro def-) ;;; TODO: For every argument a function that produces an element...
