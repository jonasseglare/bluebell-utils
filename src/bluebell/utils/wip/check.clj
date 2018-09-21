(ns bluebell.utils.wip.check
  (:require [clojure.spec.alpha :as spec]))

;; Runtime checking utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Function IO validation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::check (spec/alt :s-expr seq?
                            :spec (spec/cat :spec (complement seq?)
                                            :expr any?)))

(spec/def ::checks (spec/spec (spec/* ::check)))

(spec/def ::check-io-args (spec/cat
                           :group
                           (spec/spec
                            (spec/cat
                                    
                             :flag (spec/? any?)
                             :pre (spec/?
                                  (spec/cat :prefix #{:pre}
                                            :checks ::checks))
                             :post (spec/?
                                   (spec/cat :prefix #{:post}
                                             :symbol symbol?
                                             :checks ::checks))))
                           :body (spec/* any?)))

(defn- generate-check [[check-type check-data]]
  (case check-type
    :s-expr `(assert ~check-data)
    :spec `(assert (spec/valid? ~(:spec check-data)
                                ~(:expr check-data))
                   (do
                     (println
                      (str "In '" (quote ~check-data) "':"
                           (spec/explain-str
                            ~(:spec check-data)
                            ~(:expr check-data))))
                     (quote ~check-data)))))

(defn- generate-checking-code [checks]
  (map generate-check checks))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro check-io
  "Alternative to pre post conditions"
  [& args]
  (let [parsed (spec/conform ::check-io-args args)]
    (when (= parsed ::spec/invalid)
      (spec/explain ::check-io-args args)
      (throw (ex-info "Failed to parse args to check-fn-io"
                      {:args args})))
    (let [{:keys [group body]} parsed
          flag (if (contains? group :flag)
                 (:flag group)
                 true)
          in (:pre group)
          out (:post group)
          out-sym (or (:symbol out) (gensym "result"))]
      (if (eval flag)
        `(do
           ~@(generate-checking-code (:checks in))
           (let [~out-sym (do ~@body)]
             ~@(generate-checking-code (:checks out))
             ~out-sym))
        `(do ~@body)))))
