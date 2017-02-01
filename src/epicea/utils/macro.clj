(ns epicea.utils.macro
  (:require [clojure.spec :as spec]))

;; General error reporting for macros
(defn error [& s]
  (throw (RuntimeException. (apply str s))))

(spec/def ::if-sym #(= 'if %))
(spec/def ::expr (constantly true))
(spec/def ::if-form (spec/cat :if-sym ::if-sym
                              :test ::expr
                              :on-true ::expr
                              :on-false (spec/? ::expr)))

(spec/def ::binding (spec/cat :symbol symbol?
                              :expr ::expr))
(spec/def ::bindings (spec/spec (spec/* ::binding)))
(spec/def ::form (constantly true))
(spec/def ::forms (spec/* ::form))
(spec/def ::let-symbol (constantly true)); #(= `let %))

(spec/def ::basic-let-form (spec/cat
                            :let-symbol ::let-symbol
                            :bindings ::bindings
                            :forms ::forms))

(spec/def ::loop-symbol (constantly true))

(spec/def ::loop-form (spec/cat
                       :loop-symbol ::loop-symbol
                       :bindings ::bindings
                       :forms ::forms))

(spec/def ::fn-symbol (constantly true))
(spec/def ::fn-name symbol?)

(spec/def ::fn-args (spec/spec
                     (spec/coll-of symbol?)))


(spec/def ::fn-arity (spec/spec
                      (spec/cat
                       :args ::fn-args
                       :forms ::forms)))


(spec/def ::fn-form (spec/cat
                     :fn-symbol ::fn-symbol
                     :fn-name (spec/? ::fn-name)
                     :fn-arities (spec/* ::fn-arity)))

(spec/def ::type (constantly true))

(spec/def ::finally-symbol #(= % 'finally))
(spec/def ::catch-symbol #(= % 'catch))

(spec/def ::catch-form (spec/spec
                        (spec/cat
                         :catch-symbol ::catch-symbol
                         :type ::type
                         :var-name symbol?
                         :forms ::forms)))

(spec/def ::finally-form (spec/spec
                          (spec/cat
                           :finally-symbol ::finally-symbol
                           :forms ::forms)))

(spec/def ::non-catch #(and (not (spec/valid? ::catch-form %))
                            (not (spec/valid? ::finally-form %))))

(spec/def ::try-form (spec/cat
                      :try-symbol symbol?
                      :forms (spec/* ::non-catch)
                      :catch-forms (spec/* ::catch-form)
                      :finally-form (spec/? ::finally-form)))

(defn compare-symbols [a b]
  (try
    (let [ar (resolve a)
          br (resolve b)]
    (and (= ar br) (not (nil? ar)) (not (nil? br)))
    (catch Throwable _ false)))

(def special-forms {'if :if ; OK
                    'do :do ;; OK
                    'let* :let ;; OK
                    'loop* :loop ;; OK
                    'recur :recur ;; OK
                    'throw :throw ;; OK
                    'def :def ;; ?
                    'var :var ;; ?
                    'monitor-enter :monitor-enter
                    'monitor-exit :monitor-exit
                    'fn* :fn ;; OK
                    'try :try ;; OK
                    'catch :catch ;; OK
                    'quote :quote ;; OK
                    })
