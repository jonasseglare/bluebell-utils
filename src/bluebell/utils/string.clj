(ns bluebell.utils.string
  (:require [bluebell.utils.debug :as debug]))

(defn join-by [x]
  (fn [a b] (str a x b)))

(defn join-strings [sep args]
  (reduce (join-by sep) args))

(defn join-lines [x]
  (join-strings "\n" x))
