(ns bluebell.utils.render-text
  (:require [bluebell.utils.dsl :as dsl]
            [clojure.spec.alpha :as spec]))

(declare add-string)
(declare add-line)


(spec/def ::line-prefix string?)
(spec/def ::indent-step string?)
(spec/def ::output string?)
(spec/def ::context (spec/keys :req-un [::line-prefix
                                        ::indent-step]))
(spec/def ::accumulator (spec/keys :req-un [::output]))

;;;------- Implementation -------


(defn- import-text [x]
  (if (string? x)
    (add-line x)
    x))

(def empty-context {:line-prefix "\n"
                    :indent-step "  "})

(def empty-accumulator {:output ""})


(def eval-dsl (dsl/dsl-evaluator {:import-fn import-text
                                  :context-spec ::context
                                  :accumulator-spec ::accumulator}))

(defn- indent-context [context]
  (update context :line-prefix str
          (:indent-step context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;------- The DSL -------

(defn add-string [s]
  {:pre [(string? s)]}
  (fn [context acc]
    (update acc :output str s)))

(defn add-line [s]
  {:pre [(string? s)]}
  (fn [context acc]
    (update acc :output str (:line-prefix context) s)))

(defn indent [& body]
  (fn [context acc]
    (eval-dsl (indent-context context)
              acc
              body)))

;;;------- Main evaluation -------

(defn evaluate [input]
  (:output (eval-dsl empty-context
                     empty-accumulator input)))

;; Example: (println (evaluate ["begin" (indent "Mjao;") "end"]))
