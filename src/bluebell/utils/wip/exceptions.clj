(ns bluebell.utils.wip.exceptions
  (:require [clojure.spec.alpha :as spec]))

(defn try-unwrap-exinfo [g]
  (if (instance? clojure.lang.IExceptionInfo g)
    (ex-data g)
    g))

;; Wrap any uncaught exception in an ex-info with a tag.
(defmacro tag [key & body]
  `(try
     ~@body
     (catch Throwable e#
       (throw (ex-info
               "Tagged"
               {::tag ~key ::value (try-unwrap-exinfo e#)})))))


(spec/def ::case (spec/cat :binding symbol?
                           :expr (constantly true)))
(spec/def ::cases (spec/* ::case))

(defn case-to-pair [{binding :binding
                     expr :expr}]
  [(keyword binding) binding])

(defn wrap-throwable [x]
  (if (instance? Throwable x)
     x (ex-info "bluebell.utils.wip.exceptions/throw"
                (if (map? x)
                  x {::value x}))))

;; Throw any value
(defn throw-any [x]
  (throw (wrap-throwable x)))


(defn expand-cases [all-cases cases]
  (if (empty? cases)
    `(throw-any ~(into {} (map case-to-pair all-cases)))
    (let [g (gensym)
          {binding :binding
           expr :expr} (first cases)
          remaining (expand-cases all-cases (rest cases))]
      `(try
         ~expr
         (catch Throwable ~g
           (let [~binding (try-unwrap-exinfo ~g)]
             ~remaining))))))

;; Choose the first branch that doesn't fail
(defmacro either [& args]
  (let [parsed (spec/conform ::cases args)]
    (assert (not= ::spec/invalid parsed))
    (expand-cases parsed parsed)))

;; Expect something
(defn expect [f? x]
  (if (f? x) x
      (throw
       (ex-info
        "Expect failed"
        {::f? f? ::value x}))))
