(ns bluebell.utils.string
  (:require [bluebell.utils.debug :as debug]))

(defn join-by [x]
  (fn [a b] (str a x b)))

(defn join-lines [x]
  (reduce (join-by "\n") x))
