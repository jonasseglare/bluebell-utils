(ns bluebell.utils.wip.lufn
  (:require [bluebell.utils.wip.core :as core]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn add-lufn [state-atom ks f]
  {:pre [(sequential? ks)
         (fn? f)]}
  (swap! state-atom
         (fn [state]
           (reduce (fn [s k]
                     (assoc s k f))
                   state
                   ks))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro decl-lufn
  "Declare a lookup function"
  [name]
  `(let [state# (atom {::name (quote ~name)})]
     (defonce ~name ;; So that the loading order doesnt matter
       (fn
         ([] state#)
         ([key# & args#]
          (let [m# (deref state#)]
            (core/data-assert (contains? m# key#)
                              "Missing lookup function"
                              {:key key#
                               :existing-keys (keys m#)
                               :name (::name m#)})
            (let [f# (get m# key#)]
              (assert (fn? f#))
              (apply f# args#))))))))

(defmacro def-lufn
  "Define a lookup function"
  [name ks arglist & body]
  `(add-lufn (~name) ~ks (fn [~@arglist] ~@body)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Example
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (do

    (decl-lufn kattskit)
    (def-lufn kattskit [:mul] [a b] (* a b))
    (def-lufn kattskit [:add] [a b] (+ a b))


    )
  )


