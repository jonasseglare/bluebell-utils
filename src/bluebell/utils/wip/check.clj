(ns bluebell.utils.wip.check
  (:require [clojure.spec.alpha :as spec]))

;; Runtime checking utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Function IO validation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def prefices #{:when :pre :post})

(def prefix? (partial contains? prefices))

(def not-prefix? (complement prefix?))

(spec/def ::flag (spec/cat
                  :prefix #{:when}
                  :value any?))

(spec/def ::post
  (spec/cat :prefix #{:post}
            :symbol any?
            :checks ::checks))

(spec/def ::compact-post
  (spec/cat :prefix #{:post}
            :spec any?))

(spec/def ::any-post (spec/alt :post ::post
                               :compact-post ::compact-post))

(spec/def ::check (spec/alt :s-expr seq?
                            :spec (spec/cat :spec keyword?
                                            :expr any?)))

(spec/def ::checks (spec/spec (spec/* ::check)))

(spec/def ::check-io-args (spec/cat
                           :group
                           (spec/spec
                            (spec/cat
                                    
                             :flag (spec/? ::flag)
                             :pre (spec/?
                                  (spec/cat :prefix #{:pre}
                                            :checks ::checks))
                             :post (spec/? ::post)))
                           :body (spec/* any?)))

(defn- generate-check [label [check-type check-data]]
  (case check-type
    :s-expr `(assert ~check-data ~label)
    :spec `(assert (spec/valid? ~(:spec check-data)
                                ~(:expr check-data))
                   (do
                     (println
                      (str ~label ": "
                           "In '" (quote ~check-data) "':"
                           (spec/explain-str
                            ~(:spec check-data)
                            ~(:expr check-data))))
                     (quote ~check-data)))))

(defn- generate-checking-code [label checks]
  (map (partial generate-check label) checks))


(spec/def ::defn-arg (spec/cat :spec not-prefix?
                               :binding any?))

(spec/def ::defn-arg-list (spec/spec
                           (spec/cat :flag (spec/? ::flag)
                                     :args (spec/* ::defn-arg)
                                     :post (spec/? ::any-post))))

(spec/def ::single-defn (spec/cat :arg-list ::defn-arg-list
                                  :body (spec/* any?)))



(spec/def ::defn-rest (spec/alt :single ::single-defn
                                ;:multi ::multi-defn
                                ))

(spec/def ::checked-defn-args (spec/cat :function-name symbol?
                                        :doc (spec/? string?)
                                        :rest ::defn-rest))

(defn- generate-input-check [{:keys [spec binding]}]
  (when (not (or (= '_ spec)
                 (nil? spec)))
    (let [g (gensym)]
      `(let [~g ~binding]
         (when (not (spec/valid? ~spec ~g))
           (spec/explain ~spec ~g)
           (throw
            (ex-info
             ~(str "Input spec failed for '" binding "'")
             {:spec ~spec
              :binding (quote ~binding)})))))))

(defn- generate-input-checks [args]
  (mapv generate-input-check args))

(defn- normalize-post [[post-type post-data]]
  (case post-type
    :post post-data
    :compact-post (let [g (gensym "post")]
                    {:symbol g
                     :checks [[:spec {:spec (:spec post-data)
                                      :expr g}]]})
    nil nil))

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
                 (-> group :flag :value)
                 true)
          in (:pre group)
          out (:post group)
          out-sym (or (:symbol out) (gensym "result"))]
      (if (eval flag)
        `(do
           ~@(generate-checking-code "Input of check-io"
                                     (:checks in))
           (let [~out-sym (do ~@body)]
             ~@(generate-checking-code
                "Output of check-io"
                (:checks out))
             ~out-sym))
        `(do ~@body)))))

(defmacro checked-defn [& args]
  (let [parsed (spec/conform ::checked-defn-args args)]
    (when (= parsed ::spec/invalid)
      (spec/explain ::checked-defn-args args)
      (throw (ex-info "Failed to parse args to checked-defn"
                      {:args args})))
    (let [{:keys [function-name doc rest]} parsed
          _ (assert (= :single (first rest)))
          single (second rest)
          {:keys [arg-list body]} single
          {:keys [args flag post]} arg-list
          post (normalize-post post)
          should-check? (or (nil? flag)
                            (eval (:value flag)))
          post-sym (or (:symbol post) (gensym "checked-defn"))

          inner (if should-check?
                  `(do
                     ~@(generate-input-checks args)
                     (let [~post-sym (do ~@body)]
                       ~@(generate-checking-code
                          (str "Output of checked-defn '"
                               function-name "'")
                          (:checks post))
                       ~post-sym))
                  `(do ~@body))]
      `(defn ~function-name
         ~@(if doc [doc] [])
         ~(mapv :binding args)
         ~inner))))
