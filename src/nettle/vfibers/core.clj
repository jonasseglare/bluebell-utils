(ns nettle.vfibers.core
  (:require [clojure.spec :as spec]
            [nettle.utils.defmultiple :refer [defmultiple]]
            [nettle.utils.toposort :as toposort]
            [nettle.utils.access :as access]))


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

;; what primitive value it evaluates to
(def -datatype (access/key-accessor :datatype (merge settings {:valid-value? keyword?})))

;; What the node actually represents: used for dynamic dispatch
(def -nodetype (access/key-accessor :nodetype settings))
(def -expr (access/key-accessor :expr settings))
(def -args (access/key-accessor :args (merge settings {:valid-value? valid-args?})))
(def -refcount (access/key-accessor :refcount (merge settings {:valid-value? number?})))

(defn node-of-type? 
  ([type node]
   (= type (access/get node -datatype)))
  ([type] #(node-of-type? type %)))
    
(defn simple-expr? [x]
  (or (keyword? x)
      (symbol? x)
      (number? x)
      (string? x)
      (boolean? x)))

(defn primitive-expr 
  ([tp value]
   (access/build -datatype tp
                 -expr value
                 -nodetype :primitive
                 -simple? (simple-expr? value)))
  ([tp] 
   (access/build -datatype tp
                 -nodetype :primitive
                 -simple? false)))

(declare add-subexpr)

(defn add-simple-subexpr [dst key x]
  (assert (node? x))
  (assert (key? key))
  (assert (map? dst))
  (merge dst {key x
              x key}))

(defmultiple make-sym (:checked-get -datatype)
  (:default [_] (gensym)))
  

(defn add-arg [[m syms] arg]
  (if (contains? m arg)
    [m (conj syms (get m arg))]
    (let [g (make-sym arg)]
      [(add-subexpr m g arg) (conj syms g)])))

(defn add-complex-subexpr [dst key x]
  (let [args (access/get x -args)
        [dst syms] (reduce add-arg [dst []] args)]
    (merge
     dst
     {key (access/set x -args syms)
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

(defn reset-refcount [m]
  (into (empty m)
        (map (fn [[k v]]
               [k (access/remove v -refcount)])
             m)))

(defn get-refcount [x]
  (if-let [[n] ((:get-optional -refcount) x)]
    n 0))

(defn inc-ref [x]
  (access/set x -refcount (inc (get-refcount x))))

(defn inc-ref-recursive [m0 key]
  (assert (map? m0))
  (let [m (update-in m0 [key] inc-ref)
        v (get m key)]
    (if (and (= 1 (get-refcount v)) 
             (access/has? v -args))
      (reduce (fn [dst k] (inc-ref-recursive dst k))
              m (access/get v -args))
      m)))

(defn add-node [dst expr]
  (let [g (make-sym expr)]
    [(inc-ref-recursive
      (add-subexpr dst g expr) g)
     g]))

(defn make-map [dst expr]
  (cond
    (node? expr) (add-node dst expr)
    (vector? expr) (add-vector dst expr)
    (seq? expr) (add-seq dst expr)
    (map? expr) (add-coll dst expr)
    (set? expr) (add-coll dst expr)
    :default [dst expr]))     

(def get-nodetype (:checked-get -nodetype))

(defmultiple make-node (fn [node args] (get-nodetype node))
  (:primitive [x _] (access/get x -expr)))

(defn bind? [x]
  (not (or (access/get x -simple?)
           (<= (get-refcount x) 1))))


(defn clean-node-map [m]
  (into (empty m) (filter (fn [[k _]] (symbol? k))
                          m)))

(defn get-dependency-map [m]
  (into {} 
        (filter
         (fn [[k v]]
           (not (empty? v)))
         (map (fn [[k v]]
                [k (:args v)])
              m))))

(defn get-args [node]
  (if (access/has? node -args)
    (access/get node -args)
    []))

(defn make-node-recursive [m node]
  (if (symbol? node) 
    (if (bind? (get m node))
      node ;; it was bound to this symbol
      (make-node-recursive m (get m node)))
    (make-node node (map #(make-node-recursive m %)
                         (get-args node)))))
    

(defn make-bindings [order node-map]
  (transduce
   (comp (map (fn [key] [key (get node-map key)]))
         (filter (fn [[k node]] (bind? node)))
         (mapcat (fn [[k node]] [k (make-node-recursive node-map node)]))
         )
   conj
   order))

(defn expand-symbol [node-map x]
  (if (and (symbol? x) (contains? node-map x))
    (make-node-recursive node-map x)
    x))


(defn make-code [expr0]
  (let [[node-map expr] (make-map {} expr0)
        node-map (clean-node-map node-map)
        deps (get-dependency-map node-map)
        dep-sort (reverse (toposort/toposort deps))
        bindings (make-bindings dep-sort node-map)]
    (clojure.walk/postwalk
     #(expand-symbol node-map %)
     expr)))

