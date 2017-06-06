(ns epicea.utils.egraph
  (:require [clojure.spec :as spec]
            [epicea.utils.access :as access]))


(spec/def ::unique-tag (partial = ::node))

(spec/def ::node (spec/keys :req-un [::unique-tag]))

(defn node? [x]
  (spec/valid? ::node x))

(def empty-node {:unique-tag ::node})
(def settings {:default-base empty-node})

(defn key? [x]
  (or (symbol? x)
      (keyword? x)))

(defn valid-args? [x]
  (and (coll? x)
       (every? #(or (node? %)
                    (key? %))
               x)))

(def -simple? (access/key-accessor :simple? (merge settings {:valid-value? boolean?})))
(def -type (access/key-accessor :type (merge settings {:valid-value? keyword?})))
(def -expr (access/key-accessor :expr settings))
(def -args (access/key-accessor :args (merge settings {:valid-value? valid-args?})))
(def -make (access/key-accessor :make (merge settings {:valid-value? fn?})))
(def -refcount (access/key-accessor :refcount (merge settings {:valid-value? number?})))

(defn node-of-type? 
  ([type node]
   (= type (access/get node -type)))
  ([type] #(node-of-type? type %)))
    
(defn simple-expr? [x]
  (or (keyword? x)
      (symbol? x)
      (number? x)
      (string? x)
      (boolean? x)))

(defn primitive-expr [tp value]
  (access/build -type tp
                -expr value
                -simple? (simple-expr? value)))

(declare add-subexpr)

(defn add-simple-subexpr [dst key x]
  (assert (node? x))
  (assert (key? key))
  (assert (map? dst))
  (assoc dst key x))

(defn look-up-or-generate [m arg]
  [(if (contains? m arg)
     (get m arg) 
     (gensym))
   arg])

(defn add-complex-subexpr [dst key x]
  (let [args (access/get x -args)
        named-args (map #(look-up-or-generate dst %) args)]
    (merge
     (reduce (fn [dst [k v]]
               (add-subexpr dst k v))
             dst named-args)
     {key (access/set x -args (map first named-args))
      x key})))

(defn add-subexpr [dst key x]      
  (if (contains? dst key)
    dst
    (if (access/has? x -args)
      (add-complex-subexpr dst key x)
      (add-simple-subexpr dst key x))))

(declare make-map)

(defn add-vector [dst expr]
  (reduce 
   (fn [[dst result] x]
     (let [[new-map e] (make-map dst x)]
       [new-map (conj result e)]))
   [dst []] expr))

(defn add-seq [dst expr]
  (let [[m v] (add-vector dst (vec expr))]
    [m (seq v)]))

(defn add-coll [dst expr]
  (let [[m v] (add-vector dst (vec expr))]
    [m (into (empty expr) v)]))

(defn make-map [dst expr]
  (cond
    (node? expr) (add-node dst expr)
    (vector? expr) (add-vector dst expr)
    (seq? expr) (add-seq dst expr)
    (map? expr) (add-coll dst expr)
    (set? expr) (add-coll dst expr)
    :default [dst expr]))

(defn reset-refcount [m]
  (into (empty m)
        (map (fn [[k v]]
               [k (access/remove v -refcount)])
             m)))

(defn get-refcount [x]
  (if-let [[n] (access/get-optional x -refcount)]
    n 0))

(defn inc-ref [x]
  (access/set x -refcount (inc (get-refcount x))))

(defn inc-ref-recursive [m0 key]
  (assert (map? m0))
  (let [m (update-in m0 [key] inc-ref)
        v (get m key)]
    (if (= 1 (get-refcount v))
      (reduce (fn [dst k] (inc-ref-recursive dst k))
              (access/get v -args))
      m)))
      
      

;; (defn add-node-subexpressions [dst x]
;;   (add-args dst (access/get x -args))

;; (defn add-subexpressions [dst x]
;;   (if (node? x)
;;     (add-node-subexpressions dst x)
;;     (add-other

;; (defn make-expression-map [x]
;;   (reduce add-subexpressions {} x))
