(ns epicea.poly.core
  (:require [clojure.spec :as spec]
            [epicea.utils.debug :as debug]
            [epicea.utils.access :as access]
            [epicea.utils.defmultiple :refer [defmultiple]]
            [epicea.utils.optional :as optional]
            [clojure.pprint]))

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
  (spec/cat :main (spec/* ::expr)
            :rest (spec/? (spec/cat :and ::restargs-start
                                    :args ::expr))))

(defmultiple get-exprs first
  (:default [x] [])
  (:get [x] (-> x second :exprs))
  (:group [x] (-> x second)))

(defmultiple set-exprs (fn [k _] (first k))
  (:default [b _] b)
  (:get [g exprs] (access/updatex expr-x g (fn [m] (assoc m :exprs exprs))))
  (:group [g exprs] (access/setx expr-x g exprs)))

(def get-getter (access/compose expr-x (access/map-accessor :getter)))
(def predicate-fn (access/compose expr-x (access/map-accessor :fn)))

(defmultiple compile-expr-sub first
  (:default [x] x)
  (:get [x] (access/updatex get-getter x eval))
  (:predicate [x] (access/updatex predicate-fn x eval)))

(def expr-access {:get get-exprs :set set-exprs :has? (constantly true)})
(def update-exprs (access/updater expr-access))

(defn visit-exprs [root-expr post-fn]
  (post-fn
   (update-exprs 
    root-expr
    #(map (fn [e] 
            (visit-exprs e post-fn)) 
          %))))

(defn compile-exprs [expr-list] 
  (vec
   (map
    (fn [expr]
      (visit-exprs 
       expr 
       compile-expr-sub))
    expr-list)))

(def main-exprs (access/map-accessor :main))
(def rest-exprs (access/compose (access/map-accessor :rest)
                                (access/map-accessor :args)
                                access/vec1-accessor))

(def update-main-exprs (access/updater main-exprs))
(def update-rest-exprs (access/updater rest-exprs))

(defn update-arglist-exprs [arglist f]
  (-> arglist
      (update-main-exprs f)
      (update-rest-exprs f)))

(defn compile-arglist-exprs [arglist]
  (update-arglist-exprs arglist compile-exprs))


(declare get-expr-bindings-get-access)
(declare get-expr-bindings)
;;;; get-expr-bindings
(defmultiple get-expr-bindings first
  (:get [expr]
        (get-expr-bindings-get-access expr))
  (:group [expr]
          (reduce into [] (map get-expr-bindings (second expr))))
  (:binding [expr] [(second expr)])
  (:default [_] []))

(defn get-expr-bindings-get-access [expr]
  (reduce into [] (map get-expr-bindings (-> expr second :exprs))))



(declare eval-exprs-bindings)
;;;; eval-expr-bindings
;; Where 
;;  - 
(defn get-prefix [[tag k]]
  (:prefix k))

(defmultiple eval-optional (fn [expr k] (get-prefix expr))
  (:get [expr x] [((-> expr second :getter) x)])
  (:access [expr x] (access/getx-optional (-> expr second :getter) x)))

(defmultiple eval-expr-bindings 
  (fn [acc expr x] 
    (first expr))
  (:binding [dst [_ _] x] (conj dst x))
  (:predicate [dst [_ pred?] x] (if ((:fn pred?) x) dst))
  (:group [dst [_ exprs] x] (eval-exprs-bindings dst x exprs))
  (:get [dst expr x]
        (when-let [[result] (eval-optional expr x)]
          (eval-exprs-bindings dst result (:exprs (second expr))))))

(defn get-all-exprs [arglist]
  (if (contains? arglist :rest)
    (conj (:main arglist) (-> arglist :rest :args))
    (:main arglist)))

(defn eval-exprs-bindings [dst src exprs]
  (reduce 
   (fn [acc ex] 
     (if acc 
       (eval-expr-bindings 
        acc ex src)))
   dst exprs))


(defn get-exprs-bindings [exprs]
  (reduce into (map #(-> % second get-expr-bindings) exprs)))

(defn get-main-expr-bindings [arglist]
  (reduce into [] (map get-expr-bindings (:main arglist))))

(defn regroup-args [parsed-arglist args]
  (let [vargs (vec args)
        main-bindings (get-main-expr-bindings parsed-arglist)
        n (count vargs)
        m (count main-bindings)]
    (if (contains? parsed-arglist :rest)
      (if (<= m n)
        (conj (subvec vargs 0 m)
              (subvec vargs m)))
      (if (= m n)
        vargs))))


(defn get-arglist-bindings [arglist]
  (into (get-main-expr-bindings arglist)
        (get-expr-bindings (-> arglist :rest :args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Making a function

(defn into-or-nil [a b]
  (if (or (nil? a) (nil? b))
    nil
    (into a b)))

(defn compile-arg-parser [arglist]
  (let [exprs (get-all-exprs arglist)]
    (fn [args0] 
      (let [args (regroup-args arglist args0)]
        (if (= (count args) (count exprs))
          (reduce
           into-or-nil
           [] (map (fn [expr arg]
                     (eval-expr-bindings [] expr arg))
                   exprs args)))))))

(defn compile-body-fun [arglist body-forms]
  (let [arg-parser (compile-arg-parser arglist)
        bindings (get-arglist-bindings arglist)
        fform `(fn ~bindings ~@body-forms)
        handler (eval fform)]
    (fn [args]
      (if-let [values (arg-parser args)]
        (optional/optional (apply handler values))
        (optional/optional)))))

(spec/def ::poly-name symbol?)
(spec/def ::body-form (constantly true))
(spec/def ::default (spec/cat 
                     :default-key #(= % :default)
                     :fn (constantly true)))
                     
(spec/def ::method (spec/cat
                    :arglist (spec/spec ::arglist)
                    :body (spec/* ::body-form)))

(spec/def ::methods (spec/* (spec/spec ::method)))

(spec/def ::defpoly (spec/cat :poly-name ::poly-name
                              :default (spec/? ::default)
                              :methods ::methods))

(spec/def ::defpoly-extra (spec/cat :poly-name ::poly-name
                                    :methods ::methods))

(defn compile-method [method]
  (compile-body-fun 
   (compile-arglist-exprs (:arglist method))
   (:body method)))

(defn compile-methods [dp]
  (vec (map compile-method (:methods dp))))

(defn get-default-method [dp]
  (if (contains? dp :default)
    (-> dp :default :fn)
    (fn [args]
      (throw (RuntimeException. 
              (str "No defpoly method for " (:poly-name dp) " found for "
                   (with-out-str (clojure.pprint/pprint args))))))))
    
(def extra-methods (atom {}))

(defn get-extra-methods [sym]
  (let [extra (deref extra-methods)]
    (if-let [e (get extra sym)]
      e
      [])))

(defn eval-matching-method [methods args]
  (first (filter #(not (nil? %)) (map (fn [f] (f args)) methods))))

(defn defpoly-parsed [parsed]
  (let [name (:poly-name parsed)]
    `(let [methods# (compile-methods (quote ~parsed))
           default# (get-default-method (quote ~parsed))]
       (defn ~name [& args0#]
         (let [args# (vec args0#)]
           (first (or (eval-matching-method methods# args#)
                      (eval-matching-method (get-extra-methods (quote ~name)) args#)
                      (eval-matching-method [default#] args#))))))))

(defmacro defpoly [& args]
  (let [parsed (spec/conform ::defpoly args)]
    (if (= parsed ::spec/invalid)
      (throw 
       (RuntimeException. 
        (str "Failed to parse defpoly macro on "
             (spec/explain-str ::defpoly args))))
      (defpoly-parsed parsed))))

(defn defpoly-extra-parsed [parsed]
  (swap! extra-methods
         (fn [extra]
           (assoc 
            extra
            (:poly-name parsed)
            (compile-methods parsed))))
  nil)

(defmacro defpoly-extra [& args]
  (let [parsed (spec/conform ::defpoly-extra args)]
    (if (= parsed ::spec/invalid)
      (throw
       (RuntimeException. 
        (str "Failed to parse defpoly-extra macro on"
             (spec/explain-str ::defpoly-extra args))))
      (defpoly-extra-parsed parsed))))
