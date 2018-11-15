(ns bluebell.utils.ebmd
  "Example-based overloading"
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.wip.specutils :as specutils]
            [clojure.core :as c]
            [bluebell.utils.wip.pareto :as pareto]
            [bluebell.utils.wip.core :as utils]
            [bluebell.utils.wip.check :refer [check-io]]
            [clojure.set :as cljset]
            [bluebell.utils.render-text :as render-text]))

(declare filter-positive)
(declare any-arg)
(declare arg-spec?)
(declare check-valid-arg-spec)
(declare render-overload-text)
(declare re-resolve-arg-spec)
(declare arg-spec-key)
(declare promotion-path)
(declare promote-along-path)
(declare matches-arg-spec?)

(def v? (specutils/debug-validator false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::key (spec/or :complex (spec/cat :prefix keyword?
                                            :data (spec/* any?))
                         :simple keyword?))

(spec/def ::pos coll?)
(spec/def ::neg coll?)
(spec/def ::pred fn?)
(spec/def ::desc string?)
(spec/def ::spec #(or (spec/spec? %)
                        (keyword? %)))
(spec/def ::valid? boolean?)

(spec/def ::promoter fn?)

(spec/def ::promotions (spec/map-of ::key ::promoter))

(spec/def ::arg-spec (spec/keys :req-un [::pred
                                         ::pos
                                         ::neg
                                         ::key
                                         ::valid?
                                         ::desc]
                                :opt-un [::spec
                                         ::promotions]))

(spec/def ::general-arg-spec (spec/or :key ::key
                                      :arg-spec ::arg-spec))

(spec/def ::promotion-path (spec/*
                            (spec/spec
                             (spec/cat :key ::key
                                       :promoter ::promoter))))

(spec/def ::ref ::key)

(spec/def ::indirect-arg-spec
  (spec/keys :req-un [::ref]
             :opt-un [::promotions]))

(spec/def ::empty-arg-spec (spec/keys :opt-un [::promotions]))

(spec/def ::reg-value (spec/or :indirect ::indirect-arg-spec
                               :arg-spec ::arg-spec
                               :empty ::empty-arg-spec))

(spec/def ::input-arg-spec
  (spec/keys :opt-un [::pred ::pos ::neg
                      ::key ::desc ::spec]))



(spec/def ::arg-specs (spec/coll-of ::general-arg-spec))
(spec/def ::fn fn?)
(spec/def ::joint ::general-arg-spec)
(spec/def ::overload (spec/keys :req-un [::arg-specs ::fn]
                                :opt-un [::joint]))

(spec/def ::arg-binding (spec/cat :arg-spec any?
                                  :binding any?))

(spec/def ::joint-binding (spec/cat :prefix #{:joint}
                                    :arg-spec any?))

(spec/def ::def-poly-arg-list
  (spec/* (spec/alt :joint ::joint-binding
                    :arg-binding ::arg-binding)))

(defn transduce-conj! [transducer init-vec src]
  (persistent!
   (transduce
    transducer
    conj!
    (transient init-vec)
    src)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Impl
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def key? (partial spec/valid? ::key))

(def init-reg {::reg-counter 0})

(defonce arg-spec-registry (atom init-reg))

(defn reset-registry! []
  (reset! arg-spec-registry init-reg))

;; (reset-registry!)

(defn update-registered-arg-spec [old-value f]
  {:pre [(fn? f)]}
  (let [new-value (f old-value)]
    [(not= new-value old-value) new-value]))

(defn wrap-reg-arg-spec [x]
  {:pre [(v? ::general-arg-spec x)]}
  (if (key? x)
    {:ref x}
    x))

(defn- arg-spec-updater [to-add]
  (fn [old-value]
    {:post [(v? ::reg-value %)]}
    (let [to-add (wrap-reg-arg-spec to-add)
          _ (v? ::reg-value to-add)
          output (merge (reduce dissoc
                                (or old-value {})
                                [:ref :pred])
                        to-add)]
      output)))

(defn update-arg-spec-registry [k f]
  {:pre [(v? ::key k)
         (fn? f)]}
  (swap! arg-spec-registry
         (fn [reg]
           (let [old-arg-spec (get reg k)
                 [updated? new-arg-spec]
                 (update-registered-arg-spec
                  old-arg-spec f)
                 _ (assert (v? ::reg-value new-arg-spec))
                 reg (if updated?
                       (update reg ::reg-counter inc)
                       reg)]
             (assoc reg k new-arg-spec)))))

(defn register-arg-spec
  ([k arg-spec]
   {:pre [(v? ::key k)
          (v? ::general-arg-spec arg-spec)]
    :post [(v? ::general-arg-spec %)]}
   (update-arg-spec-registry
    k (arg-spec-updater arg-spec))
   arg-spec)
  ([arg-spec]
   {:pre [(v? ::arg-spec arg-spec)]}
   (register-arg-spec (:key arg-spec) arg-spec)))

(defn get-reg-counter []
  (-> arg-spec-registry
      deref
      ::reg-counter))

(defn unwrap-reg-value [x]
  {:pre [(v? ::reg-value x)]}
  (cond
    (contains? x :pred) x
    (contains? x :ref) (:ref x)
    :default (throw (ex-info "Empty arg-spec encountered" {}))))

(defn look-up-reg [k]
  (get (deref arg-spec-registry) k))

(defn look-up-deeper [k]
  (unwrap-reg-value (look-up-reg k)))

(defn resolve-arg-spec [init]
  {:pre [(v? ::general-arg-spec init)]
   :post [(v? ::arg-spec %)]}
  (loop [x init]
    (if (arg-spec? x)
      x
      (if-let [ag (look-up-deeper x)]
        (recur ag)
        (throw (ex-info "No arg-spec with key"
                        {:key x
                         :init init}))))))

(defn trace-arg-spec-chain [init]
  {:pre [(v? ::general-arg-spec init)]}
  (loop [k (arg-spec-key init)
         chain (transient [])]
    (let [v (look-up-reg k)]
      (if (arg-spec? v)
        (persistent! (conj! chain v))
        (recur (unwrap-reg-value v)
               (conj! chain v))))))

(defn lowest-key [init]
  {:pre [(v? ::key init)]}
  (loop [k init]
    (let [deeper (look-up-deeper k)]
      (if (arg-spec? deeper)
        k
        (recur deeper)))))

;;; Warning: Make sure that the arg specs don't differentiate
;;; between vectors and seqs, because internally, we store the
;;; samples in a set, meaning that vectors and seqs will be
;;; collapsed to a single element, which is possibly a seq.
;;; In short, use sequential? and avoid
;;; vector? or seq? to test args.

(defn- decorate-key-and-pred-from-spec [input-arg-spec]
  (if-let [s (:spec input-arg-spec)]
    (merge {:key [::spec s]
            :pred (specutils/pred s)} input-arg-spec)
    input-arg-spec))

(defn- validate-on-samples [arg-spec]
  (let [pred (:pred arg-spec)
        pos-failures (filter (complement pred)
                             (:pos arg-spec))
        neg-failures (filter pred (:neg arg-spec))]
    (if (and (empty? pos-failures)
               (empty? neg-failures))
      (assoc arg-spec :valid? true)
      (merge arg-spec
             {:valid? false
              :pos-failures pos-failures
              :neg-failures neg-failures}))))

(defn- decorate-desc [arg-spec]
  {:pre [(contains? arg-spec :key)]}
  (merge {:desc (str (:key arg-spec))} arg-spec))


(defn- init-overload-state [overload-sym init-samples]
  (check-io
   [:pre [(symbol? overload-sym)]]
   
   {:name overload-sym
    :init-samples init-samples
    :dirty? true
    :samples init-samples
    :arg-specs {}
    :overloads {}}))

(defn- state-arg-specs [state]
  (-> state
      :arg-specs

      ;; TODO: This is now a set!
      
      vals
      set))

(defn- mark-dirty [x]
  (assoc x :dirty? true))

(defn- unmark-dirty [x]
  (assoc x :dirty? false))

(defn- add-arg-spec [state arg-spec]
  (check-io
   [:pre [(map? state)
          ::general-arg-spec arg-spec]]
   (-> state
       mark-dirty
       (update :arg-specs conj [(arg-spec-key arg-spec) nil]))))

(defn- add-arg-specs [state arg-specs]
  (reduce add-arg-spec state arg-specs))

(defn- extend-arg-specs-with-joint [overload]
  (conj (vec (:arg-specs overload))
        (or (:joint overload)
            any-arg)))

(defn- add-overload [state overload]
  (check-io
   [:pre [(map? state)
          ::overload overload]]
   (let [arg-specs (extend-arg-specs-with-joint overload)]
     (-> state
         mark-dirty
         (add-arg-specs arg-specs)
         (assoc-in [:overloads (count arg-specs)
                    (mapv arg-spec-key arg-specs)]
                   overload)))))

(defn- rebuild-arg-spec-samples [state]
  (let [samples (:samples state)]
    (assert (set? samples))
    (update state
            :arg-specs
            (fn [arg-specs]
              (transduce-conj!
               (map (fn [[k arg-spec]]
                      {:pre [(key? k)]}
                      (let [arg-spec (resolve-arg-spec k)
                            sample-set (set
                                        (filter-positive
                                         arg-spec samples))]
                        [k (assoc
                            arg-spec :samples
                            sample-set)])))
               {}
               arg-specs)))))

(defn- cart-prod [a b]
  (transduce-conj!
   (comp (map (fn [a] (mapv (fn [b] [a b]) b)))
         cat)
   []
   a))

(defn- compare-sets [a b]
  (cond
    (= a b) :equal
    (cljset/subset? a b) :subset
    (cljset/superset? a b) :superset
    :default :disjoint))

(defn- rebuild-arg-spec-comparisons [state]
  (let [arg-specs (:arg-specs state)]
    (assert (map? arg-specs))
    (assoc
     state
     :arg-spec-comparisons
     (into {}
           (map (fn [[[ak av] [bk bv]]]
                  (assert (contains? av :samples))
                  (assert (contains? bv :samples))
                  [[ak bk] (compare-sets (:samples av)
                                         (:samples bv))])
                (cart-prod arg-specs arg-specs))))))

(defn- get-all-overload-arg-lists [state]
  (transduce-conj!
   (comp (map (fn [[arity overloads-per-arity]]
                {:pre [(number? arity)
                       (map? overloads-per-arity)]}
                (mapv first overloads-per-arity)))
         cat)
   []
   (:overloads state)))

(defn- compare-arg-lists [state a b]
  (if (= (count a) (count b))
    (let [cmps (:arg-spec-comparisons state)
          pairs (set (map (comp (partial get cmps) vector) a b))]
      (and (contains? pairs :subset)
             (not (contains? pairs :superset))))
    false))

(defn- compute-overload-dominates? [state]
  (let [arg-lists (get-all-overload-arg-lists state)]
    (transduce-conj!
     (map (fn [[arg-list-a arg-list-b]]
            [[arg-list-a arg-list-b]
             (compare-arg-lists
              state
              arg-list-a
              arg-list-b)]))
     {}
     (cart-prod arg-lists arg-lists))))

(defn- rebuild-overload-dominates? [state]
  (assoc
   state
   :overload-dominates?
   (compute-overload-dominates? state)))


(defn- resolve-all-arg-specs [state]
  (update state :arg-specs
          (fn [arg-specs]
            (transduce-conj!
             (map (fn [[k v]]
                    (if (not (v? ::key k))
                      (throw (ex-info "BAD arg spec key "
                                      {:key k
                                       :state state})))
                    [k (resolve-arg-spec k)]))
             {}
             arg-specs))))

(defn accumulate-all-samples [state]
  (let [samples (transduce-conj!
                 (comp (map (fn [[k v]]
                              [(:pos v) (:neg v)]))
                       cat
                       cat)
                 #{}
                 (:arg-specs state))]
    (assoc state
           :samples
           samples)))

(defn save-counter-value [state]
  (assoc state :registry-counter (get-reg-counter)))

(defn check-reg-counter [state]
  (if (not= (:registry-counter state)
            (get-reg-counter))
    (println
     "Warning: Registry counter changed while updating state"))
  state)

(defn- rebuild-all [state]
  (-> state
      save-counter-value
      resolve-all-arg-specs
      accumulate-all-samples
      rebuild-arg-spec-samples
      rebuild-arg-spec-comparisons
      rebuild-overload-dominates?
      check-reg-counter
      unmark-dirty))

(defn- dominates? [lookup-table
                   a b]
  (let [a-args (:arg-list a)
        b-args (:arg-list b)
        value (get lookup-table [a-args b-args])]
    (assert (boolean? value))
    value))

(defn evaluate-promotion-paths [arg-specs arg-list args]
  {:pre [(= (count arg-list) (count args))]}
  (let [output (map (fn [arg-spec-key arg]
                      {:pre [(v? ::key arg-spec-key)]}
                      (promotion-path arg-spec-key arg))
                    arg-list args)]
    (if (every? identity output)
      output)))


(defn- list-pareto-elements [state overloads args]
  (let [arg-specs (:arg-specs state)
        candidates (transduce-conj!
                    (comp (map (fn [[arg-list f]]
                                 (let [paths (evaluate-promotion-paths
                                              arg-specs
                                              arg-list
                                              args)
                                       promotion-count
                                       (transduce
                                        (map count)
                                        +
                                        0
                                        paths)]
                                   (if paths
                                     {:arg-list arg-list
                                      :f f
                                      :paths paths
                                      :args args
                                      :promotion-count
                                      promotion-count}))))
                          (filter identity))
                    []
                    overloads)
        lowest-promotion-count (transduce
                                (map :promotion-count)
                                min
                                Long/MAX_VALUE
                                candidates)]
    (pareto/elements
     (transduce
      (filter (fn [x] (= lowest-promotion-count
                         (:promotion-count x))))
      (completing pareto/insert)
      (pareto/frontier (partial
                        dominates?
                        (:overload-dominates? state)))
      candidates))))

(defn- render-candidate [c]
  (render-overload-text c))

(defn- render-ambiguous-overload-message [name candidates]
  (render-text/evaluate
   (render-text/add-line "Ambiguous overload for '" name "'")
   (render-text/break 1)
   (for [c candidates]
     (render-candidate c))))

(defn- resolve-overload-for-arity [state overloads args]
  (let [e (list-pareto-elements state overloads args)]
    (cond
      (= 0 (count e)) (throw (ex-info "No overload found"
                                      {:name (:name state)
                                       :arity (dec (count args))
                                       :args (butlast args)}))
      (< 1 (count e)) (throw (ex-info
                              (render-ambiguous-overload-message
                               (:name state) e)
                              {}))
      :default (first e))))

(defn- resolve-overload [state args]
  {:pre [(not (:dirty? state))]}
  (let [arity (count args)
        overloads (:overloads state)]
    (if-let [m (get overloads arity)]
      (resolve-overload-for-arity state m args)
      (throw (ex-info "No overload for this arity"
                      {:symbol (:name state)
                       :arity (dec arity)})))))

(defn- extend-args-with-joint [args0]
  (let [args (vec args0)]
    (conj args args)))

(defn- evaluate-overload [state args]
  (let [e (resolve-overload
           state (extend-args-with-joint args))
        arg-spec-keys (:arg-list e)
        overload (:f e)
        ppaths (:paths e)]
    (assert (= (count ppaths)
               (inc (count args))))
    (apply (:fn overload)
           (mapv promote-along-path
                 (butlast ppaths)
                 args))))

(defn- perform-special-op [state-atom args]
  (let [f (first args)]
    (case f
      ::add-overload (do
                       (swap! state-atom
                              add-overload
                              (second args))
                       true)
      ::get-state (deref state-atom)
      ::get-state-atom state-atom
      false)))

(defn dirty? [state]
  (or (:dirty? state)
      (not= (get-reg-counter)
            (:registry-counter state))))

(defn- perform-evaluation [state-atom args]
  (let [state (deref state-atom)
        state (if (dirty? state)
                (swap! state-atom rebuild-all)
                state)]
    (evaluate-overload state args)))

(def common-samples #{[] {} #{} "asdf" nil 9 :a 'a identity})

(defn make-overload-fn
  ([sym] (make-overload-fn sym common-samples))
  ([sym initial-samples]
   (let [state-atom (atom (init-overload-state
                           sym
                           initial-samples))]
     (fn [& args]
       (or (perform-special-op state-atom args)
           (perform-evaluation state-atom args))))))

(defn- reset-state [state]
  (init-overload-state (:name state)
                       (:init-samples state)))

(defn- render-overload-text [input]
  (let [signature (:arg-list input)]
    [(render-text/add-line "Overload:")
     (render-text/indent
      (render-text/pprint (vec (butlast signature)))
      (let [l (last signature)]
        (if (= l (:key any-arg))
          []
          [(render-text/add-line "Joint:")
           (render-text/pprint l)])))]))

(defn- render-arity-text [[full-arity overloads]]
  [(render-text/add-line "Arity " (dec full-arity))
   (render-text/indent
    (render-text/add-line "Number of overloads " (count overloads))
    (render-text/break 1)
    (mapv
     render-overload-text
     overloads))])

(defn- render-comparison-columns [arg-specs]
  [(render-text/add-line "")
   (if (empty? arg-specs)
     (render-text/add-line "Samples")
     (let [arg-spec (first arg-specs)]
       [(render-text/add-line (str (:key arg-spec)))
        (render-text/with-indent-step
          "| "
          (render-text/indent
           (render-comparison-columns (rest arg-specs))))]))])

(defn- render-comparison-arrows [n]
  (render-text/add-line
   (take n (repeat "v "))))

(defn- render-sample-evaluation [sample arg-specs]
  (let [evals (map (fn [arg-spec]
                     (if ((:pred arg-spec) sample)
                       "1 "
                       ". "))
                   arg-specs)]
    (render-text/add-line evals (str sample))))



(defn- shortest-path [dst b]
  (if (empty? dst)
    [b]
    (let [dst-n (count (first dst))
          b-n (count b)]
      (cond
        (= dst-n b-n) (conj dst b)
        (< dst-n b-n) dst
        :default [b]))))


(defn- promotion-path-sub [visited arg-spec x]
  (cond
    (contains? visited arg-spec) nil
    (matches-arg-spec? arg-spec x) (transient [])
    :default
    (do
      (let [visited (conj visited arg-spec)
            chain (trace-arg-spec-chain arg-spec)
            candidates (transduce
                        (comp (map :promotions)
                              (filter identity)
                              cat
                              (map (fn [kv]
                                     (let [[k v] kv]
                                       (when-let
                                           [p (promotion-path-sub
                                               visited k x)]
                                         (conj! p kv)))))
                              (filter identity))
                        (completing shortest-path)
                        nil
                        chain)]
        (case (count candidates)
          0 nil
          1 (first candidates)
          2 (throw (ex-info
                    "There are several equally long paths to promote value"
                    {:value x
                     :paths (for [c candidates]
                              (let [c (persistent! c)]
                                (conj (mapv first c)
                                      arg-spec)))})))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;------- Arg spec -------

(defn disp-arg-spec [x]
  x)

(defn import-arg-spec [input-arg-spec]
  {:pre [(spec/valid? ::input-arg-spec input-arg-spec)]
   :post [(spec/valid? ::arg-spec %)]}
  (-> (merge {:pos [] :neg []} input-arg-spec)
      decorate-key-and-pred-from-spec
      decorate-desc
      validate-on-samples
      register-arg-spec))



(defn import-and-check-arg-spec [x]
  (if (key? x)
    x
    (check-valid-arg-spec (import-arg-spec x))))

(defn try-set-key [arg-spec k]
  (merge {:key k} arg-spec))

(defn import-arg-spec-for-key-and-value [key value]
  (if (key? value)
    (register-arg-spec key value)
    (check-valid-arg-spec
     (import-arg-spec
      (merge value {:key key})))))

(defn basic-import-arg-spec [sym value]
  (if (key? value)
    value
    (:key
     (check-valid-arg-spec
      (import-arg-spec
       (try-set-key
        value
        sym))))))

(defmacro def-arg-spec [sym value]
  (cond
    (symbol? sym)
    `(def ~sym
       (basic-import-arg-spec
        ~(keyword (str *ns*) (name sym))
        ~value))

    (keyword? sym)
    `(do (import-arg-spec-for-key-and-value ~sym ~value)
         ~sym)
    
    :default (throw (ex-info "Cannot define arg-spec to this value"
                             {:value sym}))))

(defn provide-samples [arg-spec samples]
  (check-io
   [:pre [(map? arg-spec)
          (contains? arg-spec :pred)]]
   (let [pred (:pred arg-spec)]
     (-> arg-spec
         (update :pos
                 (fn [pos]
                   (into
                    (set pos)
                    (filter pred samples))))
         (update :neg
                 (fn [neg]
                   (into
                    (set neg)
                    (filter (complement pred) samples))))))))

(defn arg-spec-samples [arg-spec]
  (let [arg-spec (resolve-arg-spec arg-spec)]
    (reduce into [] [(:pos arg-spec)
                     (:neg arg-spec)])))

(defn pred [pred-fn]
  "Easy construction of an arg-spec. This should only be used for very common values, because it just uses a default set of sample values"
  {:pre [(fn? pred-fn)]}
  (provide-samples
   {:pred pred-fn}
   common-samples))

(def arg-spec? (specutils/pred ::arg-spec))

(defn filter-positive [arg-spec samples]
  (check-io
   [:pre [::general-arg-spec arg-spec
          (coll? samples)]
    :post k [(coll? k)]]
   (let [arg-spec (resolve-arg-spec arg-spec)]
     (filter (:pred arg-spec) samples))))

(defn matches-arg-spec? [arg-spec x]
  (check-io
   [:pre [(v? ::general-arg-spec arg-spec)]]
   ((:pred (resolve-arg-spec arg-spec)) x)))

;;;------- Overload -------
(defmacro declare-poly
  ([sym initial-samples]
   `(defonce ~sym (make-overload-fn (quote ~sym)
                                    ~initial-samples)))
  ([sym]
   `(defonce ~sym (make-overload-fn (quote ~sym)))))

(defmacro def-poly [sym arg-list & body]
  {:pre [(symbol? sym)]}
  (let [p (spec/conform ::def-poly-arg-list arg-list)
        all-parsed-args (utils/categorize-tuples p)
        function-args (:arg-binding all-parsed-args)]
    (if (= p ::spec/invalid)
      (throw (ex-info
              "Bad def-poly arg list"
              {}))
      `(~sym ::add-overload
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

;; Extra helper functions

(defn samples [overload-fn]
  {:pre [(fn? overload-fn)]}
  (:samples (overload-fn ::get-state)))

(defn arities [overload-fn]
  {:pre [(fn? overload-fn)]}
  (->> ::get-state
       overload-fn
       :overloads
       keys
       (map dec)
       set))

(defn poly-state [polyed-fn]
  {:pre [(fn? polyed-fn)]}
  (polyed-fn ::get-state))

(defn reset-poly! [overload-fn]
  {:pre [(fn? overload-fn)]}
  (swap! (overload-fn ::get-state-atom) reset-state))

(defn print-poly-str [poly-fn]
  {:pre [(fn? poly-fn)]}
  (let [state (poly-state poly-fn)]
    (render-text/evaluate
     (render-text/add-line "Name: " (:name state))
     (render-text/break 1)
     (render-text/add-line "Samples:")
     (render-text/indent
      (render-text/pprint (:samples state)))
     (render-text/break 1)
     (render-text/add-line "Arg specs:")
     (render-text/indent
      (render-text/pprint (-> state
                              :arg-specs
                              keys)))
     (render-text/break 1)
     (render-text/add-line "Overloads")
     (render-text/indent
      (mapv
       render-arity-text
       (:overloads state))))))

(def print-poly (comp println print-poly-str))

(defn print-arg-spec-comparison-str [samples arg-specs]
  (check-io
   [:pre [::arg-specs arg-specs]]
   (render-text/evaluate
    (render-comparison-columns arg-specs)
    (render-comparison-arrows (inc (count arg-specs)))
    (for [sample samples]
      (render-sample-evaluation sample arg-specs)))))

(def print-arg-spec-comparison (comp
                                println
                                print-arg-spec-comparison-str))

(defn print-poly-arg-spec-comparison
  [poly]
  {:pre [(fn? poly)]}
  (print-arg-spec-comparison
   (samples poly)
   (-> (poly-state poly)
       state-arg-specs)))

;;;------- Misc -------
(defn check-valid-arg-spec [arg-spec]
  {:pre [(spec/valid? ::arg-spec arg-spec)]}
  (when (not (:valid? arg-spec))
    (throw (ex-info
            (str "Invalid arg-spec '" (:desc arg-spec) "'")
            arg-spec)))
  (if (empty? (:pos arg-spec))
    (println "Warning: No posiive samples for "
             (:key arg-spec)))
  arg-spec)

(defn arg-spec-key [x]
  {:pre [(v? ::general-arg-spec x)]
   :post [(v? ::key %)]}
  (if (key? x)
    x
    (:key x)))

(defn arg-spec-pred [arg-spec]
  {:pre [(v? ::general-arg-spec arg-spec)]}
  (fn [x]
    ((:pred (resolve-arg-spec arg-spec)) x)))

(def-arg-spec any-arg (pred any?))


;;;------- Promotion -------
(defn register-promotion [dst-arg-spec promoter src-arg-spec]
  {:pre [(spec/valid? ::general-arg-spec dst-arg-spec)
         (fn? promoter)
         (spec/valid? ::general-arg-spec src-arg-spec)]}
  (let [dst-arg-spec (arg-spec-key dst-arg-spec)
        src-arg-spec (arg-spec-key src-arg-spec)]
    (update-arg-spec-registry
     dst-arg-spec
     (fn [arg-spec]
       (update
        arg-spec
        :promotions
        (fn [prom]
          (assoc
           (or prom {})
           src-arg-spec promoter))))))
  nil)

(defn promotion-path [arg-spec x]
  (if-let [result (promotion-path-sub #{} arg-spec x)]
    (persistent! result)))

(defn promote-along-path
  ([promotion-path x]
   (promote-along-path promotion-path x true))
  ([promotion-path x check?]
   {:pre [(v? ::promotion-path promotion-path)
          (boolean? check?)]}
   (loop [promotion-path promotion-path
          value x]
     (if (empty? promotion-path)
       value
       (let [[[k promoter] & r] promotion-path
             _ (assert (or (not check?)
                           (matches-arg-spec? k value)))
             next-value (promoter value)]
         (recur r next-value))))))
