(ns bluebell.utils.error-context
  (:import [bluebell.utils IErrorContext
            BasicErrorContext WrappingErrorContext
            BasicErrorValue]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ErrorValue
  (error-key [x])
  (message [x])
  (data [x]))

(defn error-value
  ([key]
   (BasicErrorValue. key (str "ErrorValue of type " key) nil))
  ([key msg]
   (BasicErrorValue. key msg nil)))

(defn error? [x]
  (or (satisfies? ErrorValue x)
      (instance? Exception x)))

(defn context []
  (BasicErrorContext.))

(defn context? [x]
  (instance? IErrorContext x))

(defn ok? [^IErrorContext c]
  (.ok c))

(defn get-error [^IErrorContext c]
  (.getError c))

(defn call [^IErrorContext c f & args]
  (if (ok? c)
    (try
      (let [result (apply f args)]
        (if (error? result)
          (.handleError c result)
          result))
      (catch Exception e
        (if (.shouldCatch c e)
          (.handleError c e)
          (throw e))))))

(defn catch-ex [c & ex]
  (assert (context? c))
  (assert (every? class? ex))
  (WrappingErrorContext.
   c
   nil
   (fn [c e]
     (or (some #(instance? % e) ex)
         (.shouldCatch c e)))))

(defn map-error [c f & args]
  (WrappingErrorContext.
   c
   (fn [c e]
     (.handleError c (apply f (into [e] args))))
   nil))

(defn export [c value]
  (if (ok? c)
    value
    (get-error c)))


;;;------- Extra -------
(extend-protocol ErrorValue
  clojure.lang.ExceptionInfo
  (error-key [x] :ex-info)
  (message [x] (ex-message x))
  (data [x] (ex-data x)))

(extend-protocol ErrorValue
  BasicErrorValue
  (error-key [x] (.getKey x))
  (message [x] (.getMessage x))
  (data [x] (.getData x)))
