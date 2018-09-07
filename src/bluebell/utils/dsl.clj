(ns bluebell.utils.dsl
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.specutils :as specutils]))

(defn iterable? [x]
  (or (sequential? x)
      (nil? x)))

(defn spec? [x]
  (or (fn? x) (spec/spec? x) (keyword? x)))


(spec/def ::import-fn fn?)
(spec/def ::context-spec spec?)
(spec/def ::ammucumlator-sepc spec?)
(spec/def ::settings (spec/keys :opt-un [::import-fn ::context-spec ::accumulator-spec]))

(def settings? (specutils/pred ::settings))

(defn get-import-fn [settings]
  {:post [()]}
  (or (:import-fn settings) identity))

(defn get-validator-for-spec [maybe-sp]
  (if maybe-sp
    (partial specutils/validate maybe-sp)
    identity))

(def get-accumulator-validator (comp get-validator-for-spec :accumulator-spec))

(def get-context-validator (comp get-validator-for-spec :context-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dsl-evaluator [settings]
  {:pre [(spec/valid? ::settings settings)]}
  (let [context-validator (get-context-validator settings)
        accumulator-validator (get-accumulator-validator settings)
        import-fn (get-import-fn settings)
        e (fn evaluator [context0 accumulator0 body0]
            (let [context (context-validator context0)
                  accumulator (accumulator-validator accumulator0)
                  body (import-fn body0)
                  output-accumulator (cond
                                       (fn? body) (body context accumulator)
                                       (iterable? body) (reduce (partial evaluator context)
                                                                accumulator
                                                                body)
                                       :default (throw (ex-info "Invalid imported body"
                                                                {:class (class body)})))]
              (accumulator-validator output-accumulator)))]
    (fn [context accumulator & body]
      (e context accumulator body))))
