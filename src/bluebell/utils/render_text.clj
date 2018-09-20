(ns bluebell.utils.render-text
  (:require [bluebell.utils.dsl :as dsl]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.wip.core :as utils]
            [clojure.string :as cljstr]
            [clojure.pprint :as pp]))

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

(defn- nested-to-str [nested]
  (apply str (utils/flatten nested)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;------- The DSL -------

(defn add-string [& s]
  (fn [context acc]
    (update acc :output str (nested-to-str s))))

(defn add-line [& s]
  (fn [context acc]
    (update acc :output str (:line-prefix context)
            (nested-to-str s))))

(defn indent [& body]
  (dsl/modifying-context eval-dsl indent-context body))

(defn add-block [& blk]
  (transduce
   (comp (map cljstr/split-lines)
         cat
         (map add-line))
   conj
   []
   (utils/flatten blk)))

(defn break [n]
  (take n (repeat (add-line ""))))

(defn pprint [x]
  (add-block (with-out-str (pp/pprint x))))

(defn with-indent-step [step & body]
  (dsl/modifying-context
   eval-dsl
   (fn [ctx]
     (assoc ctx :indent-step step))
   body))

;;;------- Main evaluation -------

(defn evaluate [& input]
  (:output (eval-dsl empty-context
                     empty-accumulator input)))

;; Example 1: (println (evaluate ["begin" (indent "Mjao;") "end"]))
;; Example 2: 
