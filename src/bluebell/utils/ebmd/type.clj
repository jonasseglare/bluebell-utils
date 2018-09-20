(ns bluebell.utils.ebmd.type
  (:require [bluebell.utils.ebmd :refer [def-arg-spec
                                         pred any-arg]])
  (:refer-clojure :exclude [any number
                            sequential set keyword
                            string coll map empty]))

;;;------- Common arg types -------

(def-arg-spec any any-arg)

(def-arg-spec number (pred number?))

(def-arg-spec sequential (pred sequential?))

(def-arg-spec set (pred set?))

(def-arg-spec keyword (pred keyword?))

(def-arg-spec string (pred string?))

(def-arg-spec nil-value (pred nil?))

(def-arg-spec coll (pred coll?))

(def-arg-spec map (pred map?))

(def-arg-spec empty (pred (fn [x] (and (coll? x)
                                       (empty? x)))))

