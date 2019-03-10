(ns bluebell.utils.ebmd.type
  (:require [bluebell.utils.ebmd :refer [def-arg-spec
                                         any-arg pred]])
  (:refer-clojure :exclude [any number
                            sequential set keyword
                            string coll map empty
                            symbol]))

;;;------- Common arg types -------

;; NOTE: Providing as first argument a symbol
;; also results in a keyword being bound to the arg spec.
(def-arg-spec any any-arg)

(def-arg-spec number (pred number?))

(def-arg-spec sequential (pred sequential?))

(def-arg-spec set (pred set?))

(def-arg-spec keyword (pred keyword?))

(def-arg-spec string (pred string?))

(def-arg-spec nil-value (pred nil?))

(def-arg-spec coll (pred coll?))

(def-arg-spec map (pred map?))

(def-arg-spec symbol (pred symbol?))

(def-arg-spec empty (pred (fn [x] (and (coll? x)
                                       (empty? x)))))

(def-arg-spec ::fn {:pred fn?
                    :pos [identity]
                    :neg [3]})
