(ns bluebell.utils.wip.error-context
  (:import [bluebell.utils IErrorContext ErrorContext])
  (:require [clojure.spec.alpha :as spec])
  (:refer-clojure :exclude [do]))


(spec/def ::arg (spec/alt :catch
                          (spec/cat :prefix #{:catch}
                                    :data (spec/coll-of symbol?))
                          
                          :handler
                          (spec/cat :prefix #{:handler}
                                    :expr any?)))

(spec/def ::args (spec/cat :context any?
                           :args (spec/* ::arg)))



#_(defn- call-sub [context exceptions fn args]
  (if (ok? context)
    (let [result (try (apply fn args)
                      (catch Exception ei
                        (if (some #(instance? % ei) exceptions)
                          (instance? top-ex ei)
                          ei
                          (throw ei))))]
      (if (error? result)
        (set-error context result)
        result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn context? [x]
  (instance? IErrorContext x))

(defn ok? [^IErrorContext context]
  (.ok context))

(defn set-error [^IErrorContext context e]
  (.setError context e))

(defn context []
  (ErrorContext.))

(defprotocol ErrorValue
  (message [obj])
  (data [obj]))

(defn error? [x]
  (satisfies? ErrorValue x))

(defn error
  ([msg] (ex-info msg {}))
  ([msg data] (ex-info msg data)))



(defmacro wrap [args & body]
  (let [parsed (spec/conform ::args args)]
    (when (= ::spec/invalid parsed)
      (throw (ex-info
              (str "Syntax error: "
                   (spec/explain-str ::args args))
              {})))

    `(wrap-sub (:context args)
               (:))))


;;;------- Other stuff -------

(extend-protocol ErrorValue
  clojure.lang.ExceptionInfo
  (message [obj] (ex-message obj))
  (data [obj] (ex-data obj)))

(extend-protocol ErrorValue
  Exception
  (message [obj] (.getMessage obj))
  (data [obj] nil))
