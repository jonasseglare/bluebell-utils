(ns bluebell.utils.core
  (:require [clojure.set]
            [clojure.spec.alpha :as spec]
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

;;; f: state x item -> [new-state transformed-item]
(defn reduce-coll-items-sub [f state coll]
  (reduce (fn [[state new-coll] x]
            (let [[new-state new-item] (f state x)]
              [new-state (conj new-coll new-item)]))
          [state (make-empty coll)]
          coll))

(defn comparable? [x]
  (instance? java.lang.Comparable x))

(defn sort-pairs-if-possible [pairs]
  (if (every? comparable? (map first pairs))
    (sort-by first pairs)
    pairs))

(defn sort-if-possible [x]
  (if (every? comparable? x)
    (sort x)
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
(defn reduce-coll-items [f state coll]
  (let [[new-state new-coll] (reduce-coll-items-sub
                              f state (normalize-coll coll))]
    [new-state
     (denormalize-coll coll new-coll)]))

(defn normalized-coll-accessor
  ([] {:desc "Normalized coll accessor"})
  ([x] (if (coll? x)
         (normalize-coll x)
         []))
  ([x y] (if (coll? x)
           (denormalize-coll x y)
           x)))

;;;;;;;;;;;;;;;;;;; Helpers
(defn only-visit [x? v]
  (fn [x]
    (if (x? x) (v x) x)))


(defn stateless [f]
  (fn [state x]
    [state (f x)]))


;;;;;;;;;;;;;;;;;;;;;;; Traversal
(declare traverse-postorder-cached-sub)

(defn traverse-postorder-cached-coll [m expr cfg]
  (reduce-coll-items
   (fn [m x]
     (traverse-postorder-cached-sub m x cfg))
   m
   expr))

(defn descend? [cfg expr]
  (let [d? (:descend? cfg)]
    (or (not d?) (d? expr))))

(defn register-cached [orig cfg [m new-value]]
  [(assoc m orig [new-value 1]) new-value])

(defn look-up-and-inc [m expr]
  (let [[dst n] (get m expr)]
    [(assoc m expr [dst (inc n)]) dst]))

(spec/def ::visit fn?)
(spec/def ::traverse-config (spec/keys :req-un [::visit]
                                       :opt-un [::access-coll]))

(defn coll-accessor
  ([] {:desc "coll-accessor"})
  ([x] (if (coll? x) x []))
  ([x new-val] (if (coll? x) new-val x)))

(defn traverse-postorder-cached-sub
  [m expr cfg]
  (if (contains? m expr)
    (look-up-and-inc m expr)
    (register-cached
     expr
     cfg (let [c ((:access-coll cfg) expr)
               [m c] (traverse-postorder-cached-coll m c cfg)]
           [m ((:visit cfg) ((:access-coll cfg) expr c))]))))

(def normalized-coll-accessor)

(def default-traverse-cfg {:access-coll normalized-coll-accessor})

(defn traverse-postorder-cached
  ([m expr cfg]
   (traverse-postorder-cached-sub m expr (merge default-traverse-cfg cfg)))
  ([expr cfg]
   (second (traverse-postorder-cached {} expr cfg))))
(spec/fdef traverse-postorder-cached :args
           (spec/cat
            :m (spec/? map?)
            :expr (constantly true)
            :cfg ::traverse-config))

;;;;;;;;;;;;;;;;;;;;; Traverse with state
(defn map-with-state [f state data]
  (reduce
   (fn [[state dst] x]
     (let [[state y] (f state x)]
       [state (conj dst y)]))
   [state []] data))

(defn traverse-postorder-with-state-sub [state expr visit access]
  (let [[state children] (map-with-state
                          (fn [state x]
                            (traverse-postorder-with-state-sub
                             state x visit access))
                          state (access expr))
        _ (do (println "expr=" expr)
              (println "children=" children))
        expr (access expr children)]
    (visit state expr)))

(defn traverse-postorder-with-state
  [state expr cfg]
  (let [c (merge default-traverse-cfg cfg)]
    (traverse-postorder-with-state-sub
     state expr (:visit c) (:access-coll c))))
