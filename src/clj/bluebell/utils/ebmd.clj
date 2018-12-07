(ns bluebell.utils.ebmd
  (:import [bluebell.utils.ebmd
            Registry
            Settings
            Impl
            Signature
            ArgSpec
            PolyFn
            Promotion])
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.wip.core :as utils]
            [bluebell.utils.wip.specutils :as specutils]
            [bluebell.utils.render-text :as render-text]))

(spec/def ::arg-binding (spec/cat :arg-spec any?
                                  :binding any?))

(spec/def ::joint-binding (spec/cat :prefix #{:joint}
                                    :arg-spec any?))

(spec/def ::def-poly-arg-list
  (spec/* (spec/alt :joint ::joint-binding
                    :arg-binding ::arg-binding)))

(spec/def ::pos coll?)
(spec/def ::neg coll?)
(spec/def ::pred fn?)
(spec/def ::spec any?)
(spec/def ::key any?)

(spec/def ::arg-spec (spec/keys :req-un [::pos
                                         ::neg
                                         ::key]
                                :opt-un [::pred ::spec
                                         ::reg-spec?]))

(defn- symbol-to-key [x]
  (if (symbol? x)
    (keyword (str *ns*) (name x))
    x))

(defonce ^:dynamic registry (Registry. (.release (Settings.))))

(defn- pred-from-spec [arg-spec]
  (if (contains? arg-spec :spec)
    (if (contains? arg-spec :pred)
      (throw (ex-info "An arg-spec cannot contain both :pred and :spec"
                      arg-spec))
      (assoc arg-spec :pred (partial spec/valid?
                                     (:spec arg-spec))))
    arg-spec))

(defn make-implementation [args]
  (let [m (first args)]
    (let [arg-specs (:arg-specs m)]
      (Impl. (Signature. (object-array arg-specs)
                         (:joint m))
             (:fn m)))))

(defn add-implementation [poly args]
  (.addImplementation
   poly
   (make-implementation args)))

(defn poly-fn [poly-obj]
  (fn [& args]
    (if (= ::special (first args))
      (let [[_ tp & args] args]
        (case tp
          ::add (add-implementation poly-obj args)
          ::get poly-obj
          ::summary (.renderPolySummary poly-obj)))
      (.call poly-obj (object-array args)))))

(def PolyFn? (partial instance? PolyFn))

(defn- render-comparison-columns [arg-specs]
  [(render-text/add-line "")
   (if (empty? arg-specs)
     (render-text/add-line "Samples")
     (let [arg-spec (first arg-specs)]
       [(render-text/add-line (str arg-spec))
        (render-text/with-indent-step
          "| "
          (render-text/indent
           (render-comparison-columns (rest arg-specs))))]))])

(defn- render-comparison-arrows [n]
  (render-text/add-line
   (take n (repeat "v "))))

(defn- render-sample-evaluation [sample arg-specs]
  (let [evals (map (fn [arg-spec]
                     (if (.evaluate arg-spec sample)
                       "1 "
                       ". "))
                   arg-specs)]
    (render-text/add-line evals (str sample))))

(declare import-arg-spec)

(defn def-arg-spec-sub [k m]
  (let [m (if (map? m) (merge m {:key k}) m)]
    (if (map? m)
      (let [m (merge m {:key k})]
        (import-arg-spec m)

        ;; Does not work to use spec/def in a function...
        #_(when (:reg-spec? m)
          (println "DEFINE SPEC FOR " k)
          (spec/def k (or (:spec m)
                          (:pred m))))
        m)
      (do (.registerIndirection registry k m)
          m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn import-arg-spec [arg-spec]
  (specutils/validate ::arg-spec arg-spec)
  (let [arg-spec (pred-from-spec arg-spec)
        imp (ArgSpec. (:pred arg-spec)
                      (set (:pos arg-spec))
                      (set (:neg arg-spec)))
        k (:key arg-spec)]
    (.registerArgSpec registry k imp)
    k))

(defmacro def-arg-spec [sym m]
  (let [k (symbol-to-key sym)]
    `(let [r# (def-arg-spec-sub ~k ~m)]
       ~@(if (symbol? sym)
           [`(def ~sym ~k)]
           [])
       ~k)))

(defn resolve-arg-spec [x]
  (if (instance? ArgSpec x)
    x
    (.resolve registry x)))

(defn import-arg-spec-if-needed [x]
  (try
    (resolve-arg-spec x)
    x
    (catch Exception e
      (import-arg-spec x))))

(defn arg-spec-samples [x]
  (let [as (resolve-arg-spec x)]
    (reduce into #{} [(.getPositive as)
                      (.getNegative as)])))

(defn poly-arg-specs [poly]
  (.getAllArgSpecs (poly ::special ::get)))

(defn poly-samples [poly]
  (transduce
   (comp (map arg-spec-samples)
         cat)
   conj
   #{}
   (poly-arg-specs poly)))

(defn poly-arities [poly]
  (.getArities (poly ::special ::get)))

(defn matches-arg-spec? [arg-spec x]
  (.evaluate (resolve-arg-spec arg-spec) x))

(defn arg-spec-keys []
  (.getArgSpecKeys registry))

(defmacro declare-poly [poly-name]
  {:pre [(symbol? poly-name)]}
  `(let [poly# (PolyFn. registry (quote ~(symbol (str *ns*)
                                                 (str poly-name))))]
     (def ~poly-name (poly-fn poly#))))

(defn provide-samples [dst-poly samples]
  {:pre [(PolyFn? dst-poly)
         (set? samples)]}
  (.provideSamples dst-poly samples))

(defmacro def-poly [sym arg-list & body]
  {:pre [(symbol? sym)]}
  (let [p (spec/conform ::def-poly-arg-list arg-list)
        all-parsed-args (utils/categorize-tuples p)
        function-args (:arg-binding all-parsed-args)]
    (if (= p ::spec/invalid)
      (throw (ex-info
              "Bad def-poly arg list"
              {}))
      `(~sym ::special ::add
        (merge
         {:arg-specs ~(mapv :arg-spec function-args)
          :fn (fn [~@(mapv :binding function-args)]
                ~@body)}
         ~(if-let [j (-> all-parsed-args :joint first :arg-spec)]
            {:joint j}
            {}))))))

(defmacro declare-def-poly [symbol & args]
  `(do
     (declare-poly ~symbol)
     (def-poly ~symbol ~@args)))

(defn poly-summary-str [poly]
  {:pre [(fn? poly)]}
  (poly ::special ::summary))

(def poly-summary (comp println poly-summary-str))

(def default-promotion-cost 1.0)

(defn register-promotion
  ([dst-arg-spec promoter src-arg-spec]
   (register-promotion
    dst-arg-spec
    promoter
    src-arg-spec
    default-promotion-cost))
  ([dst-arg-spec promoter src-arg-spec cost]
   (.registerPromotion
    registry
    dst-arg-spec
    (Promotion. promoter cost)
    src-arg-spec)))



(defn print-arg-spec-comparison-str
  ([arg-specs]
   (print-arg-spec-comparison-str
    (transduce
     (comp (map arg-spec-samples)
           cat)
     into
     #{}
     arg-specs)
    arg-specs))
  ([samples arg-specs]
   (render-text/evaluate
    (render-comparison-columns arg-specs)
    (render-comparison-arrows (inc (count arg-specs)))
    (for [sample samples]
      (render-sample-evaluation
       sample (map resolve-arg-spec arg-specs))))))

(def print-arg-spec-comparison (comp
                                println
                                print-arg-spec-comparison-str))

(def-arg-spec any-arg {:pred (constantly true)
                       :pos [1 2 3 4 :a {:a 3}]
                       :neg []})

(def common-values [#{1 3} {:a 3} 3 4 :a :b {} #{} [3 45]])

(defn pred
  ([f]
   (pred f common-values))
  ([f vs]
   {:pred f
    :pos (vec (filter f vs))
    :neg (vec (filter (complement f) vs))}))

(defn arg-spec-pred [as]
  (partial matches-arg-spec? as))




;;;------- Union arg specs -------
(defn extend-arg-spec [dst & extensions]
  (doseq [e extensions]
    (.extendArgSpec registry dst e)))

(defn def-arg-spec-union [key & extensions]
  (.registerArgSpecUnion registry key)
  (doseq [e extensions]
    (.extendArgSpec registry key e)))


