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

(defn add-complex-subexpr [dst key x]
  (let [args (access/get x -args)
        named-args (map (fn [x] [(gensym) x]) args)]
    (assoc (reduce (fn [dst [k v]]
                     (add-subexpr dst k v))
                   dst named-args)
           key (access/set x -args (map first named-args)))))

(defn add-subexpr [dst key x]           
  (if (access/has? x -args)
    (add-complex-subexpr dst key x)
    (add-simple-subexpr dst key x)))

(declare make-map)

(defn add-vector [dst expr]
  (reduce 
   (fn [[dst result] x]
     (let [[new-map e] (make-map dst x)]
       [new-map (conj result e)]))
   [dst []] expr))

(defn add-node [dst expr]
  (let [sym (gensym)]
    [(add-subexpr dst sym expr) sym]))

(defn make-map [dst expr]
  (cond
    (node? expr) (add-node dst expr)
    (vector? expr) (add-vector dst expr)
    :default [dst expr]))

;; (defn add-node-subexpressions [dst x]
;;   (add-args dst (access/get x -args))

;; (defn add-subexpressions [dst x]
;;   (if (node? x)
;;     (add-node-subexpressions dst x)
;;     (add-other

;; (defn make-expression-map [x]
;;   (reduce add-subexpressions {} x))
