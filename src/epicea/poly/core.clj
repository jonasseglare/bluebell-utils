(ns epicea.poly.core
  (:require [clojure.spec :as spec]
            [epicea.utils.access :as access]))

;;;;;; Spec

(spec/def ::get (spec/cat :prefix #(contains? #{:get :access} %)
                          :getter (constantly true)
                          :exprs (spec/+ ::expr)))

(spec/def ::predicate (spec/cat :prefix #(= % :pred)
                                :fn (constantly true)))

(spec/def ::restargs-start #(= '& %))
(spec/def ::binding #(and (symbol? %) (not= '& %)))
(spec/def ::expr (spec/or :binding ::binding
                          :predicate ::predicate
                          :get ::get
                          :group ::exprs))
(spec/def ::exprs (spec/coll-of ::expr))

(spec/def ::arglist 
  (spec/cat :main (spec/* (spec/or :expr ::expr
                                   :exprs ::exprs))
            :rest (spec/? (spec/cat :and ::restargs-start
                                    :args ::expr))))

;;;; get-expr-bindings
(defmulti get-expr-bindings first)

(defn get-expr-bindings-get-access [expr]
  (reduce into [] (map get-expr-bindings (-> expr second :exprs))))

(defmethod get-expr-bindings :get [expr]
  (get-expr-bindings-get-access expr))

(defmethod get-expr-bindings :group [expr]
  (reduce into [] (map get-expr-bindings (second expr))))

(defmethod get-expr-bindings :binding [expr] [(second expr)])

(defmethod get-expr-bindings :default [_] [])

;;;; eval-expr-bindings
(defmulti eval-expr-bindings (fn [dst x] (first x)))

;(defmethod eval-expr-bindings :get [expr]
;  (let [body (second expr)]
;    (if-let [[result]] (eval-optional (:prefix expr




(defn get-exprs-bindings [exprs]
  (reduce into (map #(-> % second get-expr-bindings) exprs)))

(defn get-arglist-bindings [arglist]
  (into (get-exprs-bindings (:main arglist)) 
        (get-expr-bindings (-> arglist :rest :args))))
   

;; (defn get-exprs [main]
;;   (if (contains? main :expr)
;;     [(:expr main)]
;;     (:exprs main)))

;; (defn get-expr-bindings [acc expr]
;;   acc)

;; (defn get-main-bindings [exprs]
;;   (reduce get-expr-bindings [] exprs))

;; (defn list-arglist-bindings [arglist]
;;   (concat (-> arglist :main get-exprs get-main-bindings)
;;           (get-expr-bindings [] (-> arglist :rest :args))))
  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main impl
(declare multi-fn)

(def empty-method-list [])

(defn add-method [obj x]
  (conj obj x))

(def ^:private method-map (atom {}))

(defn polyfun [name default-impl]
  (fn [& args]
    (loop [impls (get (deref method-map) name)]
      (if (empty? impls)
        (apply default-impl args)
        (if-let [[result] (apply (first impls) args)]
          result
          (recur (rest impls)))))))

(defmacro declpoly [name default-impl]
  (swap! method-map #(assoc % name empty-method-list))
  `(def ~name ~(polyfun name (eval default-impl))))

(defn make-method [arglist body-forms]
  (eval `(fn [~@arglist] [(do ~@body-forms)])))

(defn register-poly [m name arglist body-forms]
  (update-in m [name] #(add-method % (make-method arglist body-forms))))

(defmacro defpoly [name arglist & body-forms]
  (swap! method-map #(register-poly % name arglist body-forms))
  nil)

;; (declpoly rulle (fn [& args] :no-impl))
;; (defpoly rulle [x] (* x x))
