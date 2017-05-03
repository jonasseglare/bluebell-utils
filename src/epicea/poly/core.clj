(ns epicea.poly.core
  (:require [clojure.spec :as spec]
            [epicea.utils.debug :as debug]
            [epicea.utils.access :as access]
            [epicea.utils.defmultiple :refer [defmultiple]]))

;;;;;; Spec

(def keys-to-eval #{:getter :fn})

(def expr-x (access/vector-accessor 1 {:default-parent [nil nil]}))
(def exprs-x (access/map-accessor :exprs {}))
(def expr-exprs (access/compose expr-x exprs-x))

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

(defmultiple get-exprs first
  (:binding [_] [])
  (:predicate [_] [])
  (:get [x] (-> x second :exprs))
  (:group [x] (-> x second)))

(defmultiple set-exprs (fn [k _] (first k))
  (:binding [b _] b)
  (:predicate [p _] p)
  (:get [g exprs] (access/updatex expr-x (fn [m] (assoc m :exprs exprs))))
  (:group [g exprs] (access/setx expr-x exprs)))
  

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
;; Where 
;;  - 
(defmulti eval-expr-bindings (fn [acc expr x] (first expr)))

(defn get-prefix [[tag k]]
  (:prefix k))
(defmulti eval-optional (fn [expr k] (get-prefix expr)))

(defmethod eval-optional :get [expr x]
  [((-> expr second :getter) x)])

(defmethod eval-optional :access [expr x]
  (access/getx-optional (-> expr second :getter) x))

(defmethod eval-expr-bindings :binding [dst [_ _] x] 
  (conj dst x))

(defmethod eval-expr-bindings :predicate [dst [_ pred?] x]
  (if ((:fn pred?) x) dst))

(defn eval-exprs-bindings [dst src exprs]
  (reduce 
   (fn [acc ex] 
     (if acc 
       (eval-expr-bindings 
        acc ex src)))
   dst exprs))

(defmethod eval-expr-bindings :group [dst [_ exprs] x]
  (eval-exprs-bindings dst x exprs))

(defmethod eval-expr-bindings :get [dst expr x]
  (when-let [[result] (eval-optional expr x)]
    (eval-exprs-bindings dst result (:exprs (second expr)))))

(defn get-exprs-bindings [exprs]
  (reduce into (map #(-> % second get-expr-bindings) exprs)))

(defn get-arglist-bindings [arglist]
  (into (get-exprs-bindings (:main arglist)) 
        (get-expr-bindings (-> arglist :rest :args))))

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
