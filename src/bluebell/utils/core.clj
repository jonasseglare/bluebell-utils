(ns bluebell.utils.core
  (:require [clojure.set]
            [clojure.spec.alpha :as spec]
            [clojure.pprint :as pprint]
            [bluebell.utils.debug :as debug]
            [bluebell.specs.indent :as indent-spec]
            [clojure.spec.test.alpha :as stest]))
            

(defn flatten-map-hierarchy [mh]
  (reduce
   into {}
   (map 
    (fn [k m]
      (map (fn [[k2 v]] [[k k2] v])
           (vec m)))
    (keys mh)
    (vals mh))))


(defn map-map [f m]
  (into {} (map f m)))

(defn map-vals [f m]
  (map-map (fn [[k v]] [k (f v)]) m))

(defn map-keys [f m]
  (map-map (fn [[k v]] [(f k) v]) m))

(defn map-map-with-state [f state m] ;; f: state x item -> [new-state new-item]
  (reduce 
   (fn [[state m] kv-pair]
     (let [[new-state new-item] (f state kv-pair)]
       [new-state (conj m new-item)]))
   [state {}]
   m))

(defn map-vals-with-state [f state m]
  (map-map-with-state
   (fn [state [k v]]
     (let [[new-state new-value] (f state v)]
       [new-state [k new-value]]))
   state m))
   

(defn map-with-keys? [x ks]
  (if (map? x)
    (= ks (clojure.set/intersection (set (keys x)) ks))))

(defn contains-keys?
  ([ks]
   #(contains-keys? % ks))
  ([x ks]
   (and (map? x)
        (every? #(contains? x %) ks))))

(defn compute-matrix-index [sizes indices]
  (assert (= (count sizes)
             (count indices)))
  (loop [S (vec sizes)
         I (vec indices)
         i 0]
    (if (empty? I)
      i
      (recur (pop S) (pop I) (+ (* i (last S)) (last I))))))
      
;; Macro to define an initial value and the keys
(defmacro def-map [map-name empty-map]
  `(do
     (def ~(symbol (str "empty-" map-name)) ~empty-map)
     (defn ~(symbol (str map-name "?")) [x#]
       (map-with-keys? x# ~(set (keys empty-map))))))





(defn default-arg? [x]
  (and (vector? x) (= 1 (count x))))

(defn valid-partial-args? [x]
  (if (sequential? x)
    (every? (fn [y] (or (nil? y)
                        (default-arg? y)))
            x)))

(defn merge-args [default-args0 args0]
  (loop [result []
        defargs default-args0
        args args0]
    (if (empty? defargs)
      (reduce conj result args)
      (if (default-arg? (first defargs))
        (recur (conj result (ffirst defargs))
               (rest defargs)
               args)
        (recur (conj result (first args))
               (rest defargs)
               (rest args))))))

(defn tuple-generator [n]
  (let [m (- n 1)]
    (fn [[acc p] x]
      (if (= m (count p))
        [(conj acc (conj p x)) []]
        [acc (conj p x)]))))

(defn form-tuples [tuple-size data]
  (first
   (reduce 
    (tuple-generator tuple-size)
    [[] []]
    data)))

(defn form-pairs [data]
  (form-tuples 2 data))

;; EXAMPLE: (def f (provide-arguments get [[{:a 1 :b 2 :c 3}] nil]))
;; EXAMPLE: (def f (provide-arguments get [nil [:a]]))
(defn provide-arguments [fun partial-args]
  (assert (valid-partial-args? partial-args))
  (fn [& args0]
    (apply fun (merge-args partial-args args0))))

(defmacro pipe [& args]
  (loop [result (first args)
         functions (rest args)]
    (if (empty? functions)
      result
      (recur `(~(first functions) ~result)
             (rest functions)))))
      
(defn common-error [& args]  
  (throw (RuntimeException. (apply str args))))

(defn bundle [n]
  (let [g (atom [])]
    (fn [r]
      (fn [dst x]
        (let [next (swap! g conj x)]
          (if (= (count next) n)
            (do (reset! g [])
                (r dst next))
            dst))))))

(defn or-nil [fun]
  (fn [& args]
    (try
      (apply fun args)
      (catch Throwable _ nil))))

;; For generating code in-place
(defmacro macro-eval [code]
  (eval code))

(defn conj-map [dst key x]
  (if (contains? dst key)
    (update-in dst [key] #(conj % x))
    (assoc dst key [x])))


;;;;;;; Main
(defn map-with-state [f state data]
  (reduce
   (fn [[state dst] x]
     (let [[state y] (f state x)]
       [state (conj dst y)]))
   [state []] data))

(defn comparable? [x]
  (instance? java.lang.Comparable x))

(defn compare-somehow [a b]
  (try
    (compare a b)
    (catch Throwable _
      (compare (-> a class str)
               (-> b class str)))))

(defn sort-if-possible [x]
  (if (every? comparable? x) ;; Remove the if and always sort?
    (sort compare-somehow x)
    x))

(def sorted-keys (comp sort-if-possible keys))


;;;;;;;;;;;;;;;;;;; Helpers
(defn only-visit [x? v]
  (fn [x]
    (if (x? x) (v x) x)))


(defn stateless [f]
  (fn [state x]
    [state (f x)]))




;;;;;;;;;;;;;;;;;;;;; Asynchronous composition
(defn cb-comp2
  ([f g]
   (fn [x cb]
     (f x (fn [x] (g x cb))))))


(defn cb-comp [& funs]
  (reduce cb-comp2 funs))

(defn identity-cb [x cb]
  (cb x))

(defn cb-chain [& funs]
  (fn [x]
    ((apply cb-comp (butlast funs)) x (last funs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Other stuff


(defn apply-if [cnd f x]
  (if cnd (f x) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; practical macro
(spec/def ::with-value-init (spec/cat :symbol symbol?
                                      :value (constantly true)))

(defn conform-or-error [sp value]
  (let [x (spec/conform sp value)]
    (if (= ::spec/invalid x)
      (throw (ex-info (spec/explain-str sp value) {:spec sp :value value}))
      x)))

(defn with-value-sub [acc init args]
  (if (empty? args)
    (do
      `(let ~(reduce into [] acc) ~(:value init)))
    (with-value-sub
      (conj acc [(:symbol init) (:value init)])
      (assoc init :value (first args))
      (rest args))))

;;;;;;; Write a chain of updates to a value
(defmacro with-value [init & updates]
  (let [init (conform-or-error ::with-value-init init)]
    (with-value-sub []
      init updates)))


;; Force get
(defn fget [x k]
  (assert (contains? x k))
  (get x k))

(defmacro implies [a b]
  `(or (not ~a) ~b))

(defn abbreviate
  ([x0 maxlen]
   (let [x (str x0)]
     (if (<= (count x) maxlen)
       x
       (str (subs x 0 (- maxlen 3)) "..."))))
  ([x] (abbreviate x 30)))

(defn minus1-to-nil [x]
  (if (= -1 x) nil x))

(defn next-index-of [full part]
  (fn [index]
    (if (not (nil? index))
      (minus1-to-nil
       (.indexOf full part (inc index))))))

;; Find all substrings
(defn indices-of [full part]
  (->> (iterate (next-index-of full part) -1)
       rest
       (take-while (complement nil?))))

;; Used when debugging, to set dynamic variables to true.
(defmacro with-flags [flags & body]
  `(binding ~(reduce into [] (map (fn [x] [x true]) flags))
     ~@body))


(declare indent-nested-sub2)

(defn reduce-indented-with-prefix [result data prefix step]
  (reduce (fn [result x]
            (indent-nested-sub2 result x prefix step))
          result data))

(defn indent-nested-sub2 [result data prefix step]
  (cond
    (nil? data) (do
                  (println "Result so far:")
                  (println result)
                  (throw (ex-info "Nil not allowed!"
                                  {:result-so-far result})))
    (string? data) (str result prefix data)
    (sequential? data) (let [f (first data)]
                         (if (map? f)
                           (let [new-values (merge {:prefix prefix
                                                    :step step} f)]
                             (reduce-indented-with-prefix result
                                                          (rest data)
                                                          (:prefix new-values)
                                                          (:step new-values)))
                           (reduce-indented-with-prefix result data (str prefix step) step)))
    :default (throw (ex-info
                     (str "Cannot indent value of class "
                          (class data))
                     {}))

    ))

(defn indent-nested
  ([data] (indent-nested {:prefix "\n" :step "  "} data))
  ([settings data]
   (if (string? data)
     data
     (reduce-indented-with-prefix
      ""
      data
      (:prefix settings)
      (:step settings)))))

(defn data-assert-sub
  ([expr msg data]
   `(if (not ~expr)
      (throw (ex-info ~(str "Assertion '" expr "' failed: " msg)
                      ~data)))))

(defmacro data-assert
  ([expr msg data] (data-assert-sub expr msg data))
  ([expr data] (data-assert-sub expr "(no explanation)" data)))

(defn cond-call [arg condition f]
  (assert (boolean? condition))
  (assert (fn? f))
  (if condition
     (f arg)
     arg))


(defn default-settings-fn [settings]
  (let [default-keys (set (keys settings))]
    (fn [provided]
      (let [provided-keys (set (keys provided))
            more (clojure.set/difference
                  provided-keys default-keys)]
        (if (not (empty? more))
          (throw (ex-info "Extra keys provided" {:default-keys default-keys
                                                 :provided-keys provided-keys
                                                 :extra more})))
        (merge settings provided)))))

(defn atom-fn []
  (let [x (atom nil)]
    (fn
      ([] (deref x))
      ([value] (reset! x value) value))))

(defn first-arg [x & args]
  x)

(defn copy-to-key [dst-map src-fun dst-key]
  (assoc dst-map dst-key (src-fun dst-map)))

(defn ensure-not-nil [x]
  (assert (not (nil? x)))
  x)

(defn crash-if-called [& args]
  (throw (ex-info "This function should never be called!" {:args args})))

(def keyset (comp set keys))




(defn data-to-string
  ([data] (data-to-string data 300))
  ([data n]
   (abbreviate
    (with-out-str
      (pprint/pprint data)) n)))

(spec/def ::error-context-args (spec/cat :desc string?
                                         :data any?
                                         :body (spec/* any?)))
(defmacro error-context [& args0]
  (let [args (spec/conform ::error-context-args args0)]
    (assert (not= args ::spec/invalid))
    `(try
       ~@(:body args)
       (catch Throwable e#
         (println ~(str "Error in this context: '"
                        (:desc args) "'\n"))
         (println "  Error data:\n" (data-to-string ~(:data args)))
         (throw e#)))))


(defmacro with-value-export [f-sym & body]
  `(let [dst# (atom [])
         ~f-sym (fn [x#] (swap! dst# conj x#) x#)
         ret# (do ~@body)]
     [ret# (deref dst#)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Counting items
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn count-item
  "Used by count-values"
  ([] {})
  ([m] m)
  ([m item]
   (if (contains? m item)
     (update m item inc)
     (assoc m item 1))))

(defn count-values
  "Takes a collection and counts how many times each item occurs in the collection."
  [collection]
  (reduce
   count-item
   {}
   collection))

(defn count-or-0 [count-map x]
  (get count-map x 0))

#_(defn caching-fn []
  (let [p (promise)]
    (fn [f]
      (if (realized? p)
        (deref p)
        (let [value (f)]
          (deliver p value)
          value)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Maps
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro map-of [& symbols]
  (into {} (map (fn [s] [(keyword s) s]) symbols)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Misc
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro namespaced-keyword [x]
  `(keyword (str *ns*) ~x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Function arguments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn first-arg [& args]
  (first args))

(defn insert-at [v index extra]
  {:pre [(vector? v)
         (int? index)
         (coll? extra)]}
  (reduce into [] [(subvec v 0 index)
                   extra
                   (subvec v index)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Function IO validation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::check (spec/alt :s-expr seq?
                            :spec (spec/cat :spec keyword?
                                            :expr any?)))

(spec/def ::checks (spec/spec (spec/* ::input-check)))

(spec/def ::check-fn-io-args (spec/cat
                              :flag (spec/? any?)
                              :input-checks ::checks
                              :output-prefix #{:out}
                              :output-symbol symbol?
                              :output-checks ::checks
                              :body (spec/* any?)))

(defn- generate-check [[check-type check-data]]
  (case check-type
    :s-expr `(if (not ~check-data)
               (throw (ex-info "Check failed"
                               {:expr (quote ~check-data)})))
    :spec `(if (not (spec/valid? ~(:spec check-data)
                                 ~(:expr check-data)))
             (throw (ex-info "Spec check failed"
                             {:expr (quote ~check-data)
                              :explanation
                              (spec/explain-data
                               ~(:spec check-data)
                               ~(:expr check-data))})))))

(defn- generate-checking-code [checks]
  (map generate-check checks))

(defmacro check-fn-io
  "Alternative to pre post conditions"
  [& args]
  (let [parsed (spec/conform ::check-fn-io-args args)]
    (when (= parsed ::spec/invalid)
      (spec/explain ::check-fn-io-args args)
      (throw (ex-info "Failed to parse args to check-fn-io"
                      {:args args})))
    (let [{:keys [input-checks output-symbol
                  output-checks body]} parsed
          flag (if (contains? parsed :flag)
                 (:flag parsed)
                 true)]
      (if (eval flag)
        `(do
           ~@(generate-checking-code input-checks)
           (let [result# (do ~@body)]
             result#))
        `(do ~@body)))))
