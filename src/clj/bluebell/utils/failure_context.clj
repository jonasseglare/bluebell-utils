(ns bluebell.utils.failure-context
  (:import [bluebell.utils IFailureContext
            BasicFailureContext WrappingFailureContext
            BasicFailureValue AFailureContext])
  (:refer-clojure :exclude [do]))

(defn handle-failure
  "Should usually not be called directly. Forcefully sets the failure."
  [^IFailureContext c e]
  (.handleFailure c e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn failure-value
  "Create an failure value"
  ([key]
   (BasicFailureValue. key (str "FailureValue of type " key) nil))
  ([key msg]
   (BasicFailureValue. key msg nil))
  ([key msg data]
   (BasicFailureValue. key msg data)))

(defn get-key [^BasicFailureValue x]
  (.getKey x))

(defn failure-value?
  "Test if something is an failure value"
  ([x]
   (instance? BasicFailureValue x))
  ([k x]
   (and (instance? BasicFailureValue x)
        (= k (get-key x)))))

(defn context
  "Create a default context"
  []
  (BasicFailureContext. failure-value?))

(defn context?
  "Test if it is a context"
  [x]
  (instance? IFailureContext x))

(defn ok?
  "Returns true iff no failure"
  [^IFailureContext c]
  (.ok c))

(defn get-failure
  "Get the failure"
  [^IFailureContext c]
  (.getFailure c))

(defn catch-ex
  "Provide exceptions that should be caught"
  [c & ex]
  (assert (context? c))
  (assert (every? class? ex))
  (WrappingFailureContext.
   c
   nil
   (fn [c e]
     (or (some #(instance? % e) ex)
         (.isFailure c e)))))

(defn catch-exception [c]
  (catch-ex c Exception))

(defn mapfail
  "Map all failures to other failures using f"
  [c f & args]
  (assert (context? c))
  (WrappingFailureContext.
   c
   (fn [c e]
     (.handleFailure c (apply f (into [e] args))))
   nil))

(defn mapfail-when [c pred f & args]
  (WrappingFailureContext.
   c
   (fn [c e]
     (.handleFailure c (if (pred e)
                       (apply f (into [e] args))
                       e)))
   (fn [c e]
     (or (pred e)
         (.isFailure c e)))))

(defn constant-failure [c value]
  (WrappingFailureContext.
   c
   (fn [c _]
     (.handleFailure c value))
   nil))

(defn export
  "Return the failure or the value"
  [c value]
  (assert (context? c))
  (if (ok? c)
    value
    (get-failure c)))

(defn report-failure
  "Report an failure if it has not been done yet"
  [^AFailureContext c e]
  (.reportFailure c e))

(defn check
  "Produce failure of check fail"
  [c pred & args]
  (if (and (ok? c) (not (apply pred args)))
    (handle-failure c (failure-value :check-failed
                                 "Check failed"
                                 {:pred pred
                                  :args args}))))

(defn wrap-expect-failure
  "Default function for producing failures in `expect`"
  [pred value]
  (failure-value :expect-failed
               "Expect failed"
               {:pred pred
                :value value}))

(defn expect
  "Apply a predicate to a value, and if it is false then produce an failure"
  ([c pred value] (expect c pred value wrap-expect-failure))
  ([c pred value wrapper]
   (if (ok? c)
     (if (pred value)
       value
       (handle-failure c (wrapper pred value))))))

(defmacro with
  "Only perform the body if context ok, and take care of any failures."
  [c & body]
  `(~c (fn [] ~@body)))

(defmacro with-export
  "Just like with, but wrap export around it."
  [c & body]
  `(let [c# ~c]
     (export c# (with c# ~@body))))

(defn ignore-when
  "If an failure matches the predicate, transform it into a valid value using the provided function."
  ([c pred]
   (ignore-when c pred (constantly nil)))
  ([c pred f & args]
   (WrappingFailureContext.
    c
    (fn [c e]
      (if (pred e)
        (apply f (into [e] args))
        (.handleFailure c e)))
    nil)))

(defn describe
  "Creates function that we can use as an failure mapper to better describe things"
  ([k msg]
   (fn [e]
     (failure-value k msg e)))
  ([msg]
   (describe :description msg)))
