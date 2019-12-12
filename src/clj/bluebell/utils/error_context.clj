(ns bluebell.utils.error-context
  (:import [bluebell.utils IErrorContext
            BasicErrorContext WrappingErrorContext
            BasicErrorValue AErrorContext])
  (:refer-clojure :exclude [do]))
(defn handle-error
  "Should usually not be called directly. Forcefully sets the error."
  [^IErrorContext c e]
  (.handleError c e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn error-value
  "Create an error value"
  ([key]
   (BasicErrorValue. key (str "ErrorValue of type " key) nil))
  ([key msg]
   (BasicErrorValue. key msg nil))
  ([key msg data]
   (BasicErrorValue. key msg data)))

(defn get-key [^BasicErrorValue x]
  (.getKey x))

(defn error-value?
  "Test if something is an error value"
  ([x]
   (instance? BasicErrorValue x))
  ([k x]
   (and (instance? BasicErrorValue x)
        (= k (get-key x)))))

(defn context
  "Create a default context"
  []
  (BasicErrorContext. error-value?))

(defn context?
  "Test if it is a context"
  [x]
  (instance? IErrorContext x))

(defn ok?
  "Returns true iff no error"
  [^IErrorContext c]
  (.ok c))

(defn get-error
  "Get the error"
  [^IErrorContext c]
  (.getError c))

(defn catch-ex
  "Provide exceptions that should be caught"
  [c & ex]
  (assert (context? c))
  (assert (every? class? ex))
  (WrappingErrorContext.
   c
   nil
   (fn [c e]
     (or (some #(instance? % e) ex)
         (.isError c e)))))

(defn catch-exception [c]
  (catch-ex c Exception))

(defn maperr
  "Map all errors to other errors using f"
  [c f & args]
  (assert (context? c))
  (WrappingErrorContext.
   c
   (fn [c e]
     (.handleError c (apply f (into [e] args))))
   nil))

(defn maperr-when [c pred f & args]
  (WrappingErrorContext.
   c
   (fn [c e]
     (.handleError c (if (pred e)
                       (apply f (into [e] args))
                       e)))
   (fn [c e]
     (or (pred e)
         (.isError c e)))))

(defn constant-error [c value]
  (WrappingErrorContext.
   c
   (fn [c _]
     (.handleError c value))
   nil))

(defn export
  "Return the error or the value"
  [c value]
  (assert (context? c))
  (if (ok? c)
    value
    (get-error c)))

(defn report-error
  "Report an error if it has not been done yet"
  [^AErrorContext c e]
  (.reportError c e))

(defn check
  "Produce error of check fail"
  [c pred & args]
  (if (and (ok? c) (not (apply pred args)))
    (handle-error c (error-value :check-failed
                                 "Check failed"
                                 {:pred pred
                                  :args args}))))

(defn wrap-expect-error
  "Default function for producing errors in `expect`"
  [pred value]
  (error-value :expect-failed
               "Expect failed"
               {:pred pred
                :value value}))

(defn expect
  "Apply a predicate to a value, and if it is false then produce an error"
  ([c pred value] (expect c pred value wrap-expect-error))
  ([c pred value wrapper]
   (if (ok? c)
     (if (pred value)
       value
       (handle-error c (wrapper pred value))))))

(defmacro with
  "Only perform the body if context ok, and take care of any errors."
  [c & body]
  `(~c (fn [] ~@body)))

(defmacro with-export
  "Just like with, but wrap export around it."
  [c & body]
  `(let [c# ~c]
     (export c# (with c# ~@body))))

(defn ignore-when
  "If an error matches the predicate, transform it into a valid value using the provided function."
  ([c pred]
   (ignore-when c pred (constantly nil)))
  ([c pred f & args]
   (WrappingErrorContext.
    c
    (fn [c e]
      (if (pred e)
        (apply f (into [e] args))
        (.handleError c e)))
    nil)))

(defn describe
  "Creates function that we can use as an error mapper to better describe things"
  ([k msg]
   (fn [e]
     (error-value k msg e)))
  ([msg]
   (describe :description msg)))
