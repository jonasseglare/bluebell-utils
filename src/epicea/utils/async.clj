(ns epicea.utils.async
  (:require [clojure.core.async :as async]))

(defn producer [f init-state c]
  (async/onto-chan
   c
   (take-while (complement nil?) 
               (iterate f init-state)))
  c)

(defn exhaust [c]
  (async/<!! (async/go
               (loop [acc []]
                 (if-let [y (async/<! c)]
                   (recur (conj acc y))
                   acc)))))
               
