(ns bluebell.utils.core
  (:require [clojure.set]
            [clojure.spec.alpha :as spec]
            [clojure.pprint :as pprint]
            [bluebell.utils.debug :as debug]
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

(defn make-empty [x]
  (if (vector? x)
    [] (empty x)))

(defn reverse-if-seq [[state x]]
  [state (if (seq? x) (reverse x) x)])

;; Bug in Clojure?

(defn comparable? [x]
  (instance? java.lang.Comparable x))

(defn sort-pairs-if-possible [pairs]
  ;(println "   Sort pairs")
  (if (every? comparable? (map first pairs))
    (do
      ;(println "Keys are" (map first pairs))
      (sort-by first pairs))
    
    (do
      ;(println "CANNOT BE SORTED" (map first pairs))
      ;(throw (ex-info "This is bad" {}))
      pairs)))

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

(defn normalize-coll [coll]
  (cond
    (map? coll) (vec (apply concat (sort-pairs-if-possible (vec coll))))
    (set? coll) (sort-if-possible (vec coll))
    :default (vec coll)))

(defn make-map [proto coll]
  (first
   (reduce (fn [[m state] x]
             (if (empty? state)
               [m [x]]
               [(conj m (conj state x)) []]))
           [(make-empty proto) []] coll)))

(defn make-seq [proto coll]
  (reverse (into (make-empty proto) coll)))

(defn denormalize-coll [proto coll]
  (cond
    (map? proto) (make-map proto coll)
    (seq? proto) (make-seq proto coll)
    :default (into (make-empty proto) coll)))

;;;;;;; Main
(defn map-with-state [f state data]
  (reduce
   (fn [[state dst] x]
     (let [[state y] (f state x)]
       [state (conj dst y)]))
   [state []] data))

(def sorted-keys (comp sort-if-possible keys))

;; Access the values of a map. It has to be a map!
(defn map-vals-accessor
  ([] {:desc "map-vals-accessor"})
  ([x]
   (assert (map? x))
   (vec (map (partial get x) (sorted-keys x))))
  ([x y]
   (assert (map? x))
   (assert (= (count x)
              (count y)))
   (into x (map vector (sorted-keys x) (vec y)))))

(defn normalized-coll-accessor
  ([] {:desc "Normalized coll accessor"})
  ([x] (if (coll? x)
         (normalize-coll x)
         []))
  ([x y] (if (coll? x)
           (denormalize-coll x y)
           x)))

;; To check that the accessor is effective
(defn null-accessor
  ([] {:desc "Null accessor"})
  ([x] [])
  ([x y] x))

;;;;;;;;;;;;;;;;;;; Helpers
(defn only-visit [x? v]
  (fn [x]
    (if (x? x) (v x) x)))


(defn stateless [f]
  (fn [state x]
    [state (f x)]))


;;;;;;;;;;;;;;;;;;;;;;; Traversal
(declare traverse-postorder-cached-sub)

(defn traverse-postorder-cached-coll [m expr parent cfg]
  (map-with-state
   (fn [m x]
     (traverse-postorder-cached-sub m x cfg parent))
   m
   expr))

(defn descend? [cfg expr]
  (let [d? (:descend? cfg)]
    (or (not d?) (d? expr))))

(defn add-parent [parents parent]
  (update
   parents parent
   (fn [n] (inc (or n 0)))))

(defn register-cached [orig cfg [m new-value] parent]
  [(assoc m orig {:mapped new-value
                  :parents (add-parent {} parent)}) new-value])

(defn look-up-and-inc [m expr parent]
  (let [{dst :mapped
         parents :parents} (get m expr)]
    [(assoc m expr {:mapped dst
                    :parents (add-parent parents parent)}) dst]))

(spec/def ::visit fn?)
(spec/def ::traverse-config (spec/keys :req-un [::visit]
                                       :opt-un [::access-coll]))

(defn coll-accessor
  ([] {:desc "coll-accessor"})
  ([x] (if (coll? x) x []))
  ([x new-val] (if (coll? x) new-val x)))

(defn traverse-postorder-cached-sub
  [m expr cfg parent]
  (if (contains? m expr)
    (look-up-and-inc m expr parent)
    (register-cached
     expr
     cfg (let [c ((:access-coll cfg) expr)
               [m c] (traverse-postorder-cached-coll m c expr cfg)]
           [m ((:visit cfg) ((:access-coll cfg) expr c))])
     parent)))

(def normalized-coll-accessor)

(def default-traverse-cfg {:access-coll normalized-coll-accessor
                           :only-unique false})

(defn traverse-postorder-cached
  ([m expr cfg]
   (traverse-postorder-cached-sub m expr (merge default-traverse-cfg cfg) ::parent))
  ([expr cfg]
   (second (traverse-postorder-cached {} expr cfg))))
(spec/fdef traverse-postorder-cached :args
           (spec/cat
            :m (spec/? map?)
            :expr (constantly true)
            :cfg ::traverse-config))

;;;;;;;;;;;;; Helper utility on the cached one
(declare register-child-at)

(defn register-child-at-parents [m child at n]
  (let [parents (vec (get-in m [at :parents]))]
    (reduce
     (fn [m [k v]]
       (register-child-at m child k (* n v)))
     m
     parents)))

(defn register-child-here [m child at n]
  (update-in m [at :children child] (fn [x] (+ n (or x 0)))))

(defn register-child-at [m child at n]
  (register-child-here
   (register-child-at-parents m child at n)
   child
   at n))

(defn register-children [m]
  (reduce
   (fn [m c]
     (register-child-at m c c 1))
   m
   (keys m)))


;;;;;;;;;;;;;;;;;;;;; Traverse with state

(defn traverse-postorder-with-state-sub [state expr visit access]
  (let [[state children] (map-with-state
                          (fn [state x]
                            (traverse-postorder-with-state-sub
                             state x visit access))
                          state (access expr))
        expr (access expr children)]
    (visit state expr)))

(defn process-config [cfg]
  (merge default-traverse-cfg cfg))

(defn traverse-postorder-with-state
  ([expr cfg]
   (let [c (process-config cfg)]
     (traverse-postorder-with-state-sub
      (:state c) expr (:visit c) (:access-coll c))))
  ([state expr cfg]
   (traverse-postorder-with-state
    expr (assoc cfg :state state))))


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


(def default-subexpr-cfg (merge default-traverse-cfg {:visit identity}))

(defn insert-subexpr [result p k]
  (update result p (fn [x] (conj (or x #{}) k))))

(defn add-subexpression-to-parents-of [result lookup p k]
  (if (= p ::parent)
    result
    (let [parents (keys (get-in lookup [p :parents]))]
      (reduce
       (fn [r x]
         (add-subexpression-to-parents-of r lookup x k))
       (insert-subexpr result p k)
       parents))))

(defn add-subexpression [result lookup k]
  (add-subexpression-to-parents-of result lookup k k))

(defn compute-subexpressions-sub [analyzed]
  (reduce (fn [r k]
            (add-subexpression r analyzed k))
          {}
          (keys analyzed)))

(defn compute-subexpressions
  ([expr]
   (compute-subexpressions expr {}))
  ([expr cfg]
   (compute-subexpressions-sub
    (first
     (traverse-postorder-cached
      {} expr (merge default-subexpr-cfg cfg))))))

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

(defn indent-sub [step prefix data]
  (if (coll? data)
    (apply
     str
     (map (partial
           indent-sub
           step
           (str prefix step))
          data))
    (str prefix data)))

(defn indent-nested
  ([settings data]
   (apply
    str
    (map (partial indent-sub
                  (:step settings)
                  (:prefix settings))
         data)))
  ([data]
   (indent-nested {:prefix "\n"
                   :step "  "}
                  data)))

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
