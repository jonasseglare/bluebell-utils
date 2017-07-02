(ns bluebell.utils.async
  (:require [clojure.core.async :as async]))

(defn protect [f]
  (fn [x]
    (try
      (f x)
      (catch Throwable e
        e))))

(defn producer [f init-state c]
  (async/onto-chan
   c
   (take-while 
    (complement nil?) 
    (iterate (protect f) init-state)))
  c)

(defn exhaust [c]
  (async/<!! (async/into [] c)))
               
