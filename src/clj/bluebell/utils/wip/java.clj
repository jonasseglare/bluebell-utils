(ns bluebell.utils.wip.java)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro set-field [obj sym expr]
  {:pre [(symbol? sym)]}
  `(set! (~(symbol (str ".-" (name sym))) ~obj) ~expr))
