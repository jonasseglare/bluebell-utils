(ns bluebell.utils.mapspec
  (:require [bluebell.utils.dsl :as dsl]
            [clojure.core :as c]))


;; (defn validator [f]
;;   (fn [x]
;;     (assert (f x) "Validation failed")
;;     x))

;; (defn initialize-mapspec [name]
;;   {:name name
;;    :validator-name name
;;    :fields {}
;;    :validate '(validator map?)})

;; (defn mapspec? [x]
;;   true)

;; (defn compile-mapspec [mapspec]
;;   `(do
;;      (def ~(:validator-name mapspec) ~(:validate mapspec))))

;; (defn initialize-field [state field-name]
;;   (update-in state [:fields field-name]
;;              (fn [empty-field]
;;                (assert (nil? empty-field) (str "Field '" field-name "' already specified"))
;;                {:access-name field-name
;;                 :get-name field-name
;;                 :key (keyword field-name)})))

;; (defn field-sub [field-name body]
;;   (fn [state]
;;     (initialize-field state field-name)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;;  Interface
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmacro specify
;;   "Top-level form to specify something"
;;   [name & body]
;;   {:pre [(symbol? name)]}
;;   (compile-mapspec
;;    (dsl/do-body (initialize-mapspec name)
;;                 mapspec?
;;                 (eval (vec body)))))

;; (defmacro field
;;   "Add a field to the specification"
;;   [field-name & body]
;;   `(field-sub (quote ~field-name) ~body))
