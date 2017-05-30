(ns epicea.typed.core
  (:require [clojure.spec :as spec]
            [epicea.typed.lang :as lang]
            [epicea.utils.debug :as debug]
            [epicea.utils.core :as core]
            [epicea.utils.defmultiple :refer [defmultiple]]
            [epicea.utils.access :as access] :reload-all))

(spec/def ::unspecified nil?)

(spec/def ::dynamic (partial = :dynamic))

(spec/def ::dynamic-value (spec/cat
                     :type ::dynamic
                     :value (spec/? (constantly true))))

(defn value-or-type [type-tag value-tester?]
  (spec/or :type (partial = type-tag)
           :value value-tester?))

(spec/def ::number (value-or-type :number number?))
(spec/def ::double (value-or-type :double double?))
(spec/def ::long (value-or-type :long #(instance? java.lang.Long %)))
(spec/def ::float (value-or-type :float float?))
(spec/def ::bool (value-or-type :boll boolean?))
(spec/def ::int (value-or-type :int int?))


(spec/def ::count number?)
(spec/def ::key keyword?)

(spec/def ::record-field (spec/cat 
                          :key ::key
                          :value-type ::type))

(spec/def ::record (spec/cat
                    :type (partial = :record)
                    :fields (spec/* ::record-field)))

(spec/def ::vec (spec/cat
                 :type (partial = :vec)
                 :value-type ::type
                 :count ::count))

(spec/def ::vec-literal (spec/cat
                         :type (partial = :vecdata)
                         :data (spec/* ::type)))

(spec/def ::pointer (spec/cat
                     :type (partial = :pointer)
                     :value-type ::type
                     :at (spec/? (spec/cat 
                                  :array symbol?
                                  :offset (spec/or :int ::int
                                                   :long ::long)))))



(spec/def ::constant (spec/cat
                      :type (partial = :constant)
                      :value (constantly true)))

(spec/def ::type (spec/or :double ::double
                          :float ::float
                          :long ::long
                          :int ::int
                          :bool ::bool
                          :number ::number
                          :record ::record
                          :vec ::vec
                          :vec ::vec-literal
                          :pointer ::pointer
                          :unspecified ::unspecified ;; <-- a primitive building block
                          :constant ::constant
                          :dynamic ::dynamic
                          :dynamic ::dynamic-value
                          :dynamic symbol?))

(def pair-vec {:default-base [nil nil]})
(def type-head (access/index-accessor 0 pair-vec))
(def type-body (access/index-accessor 1 pair-vec))

(def primitive-values #{:double :float :long :int :bool :number})

(defn size-op [f]
  (fn [a b]
    (if (and (number? a) (number? b))
      (f a b))))
(def size-add +)
(def size-mul *)
(def size-max max)

(declare compute-size)

(defn vec-body? [x]
  (and (map? x)
       (contains? #{:vecdata :vec} (:type x))))


(defn vec-type [vec-body]
  (assert (vec-body? vec-body))
  (if (= :vecdata (:type vec-body))
    (-> vec-body :data first)
    (:value-type vec-body)))

(defn vec-size [vec-body]
  (assert (vec-body? vec-body))
  (if (= :vecdata (:type vec-body))
    (-> vec-body :data count)
    (:count vec-body)))

(def v0 (second (spec/conform ::type [:vecdata 1 2 3 4])))
(def v1 (second (spec/conform ::type [:vec 1 3])))
(assert (= :long (first (vec-type v0))))
(assert (= :long (first (vec-type v1))))
(assert (= 4 (vec-size v0)))
(assert (= 3 (vec-size v1)))

(defn compute-vec-size [x]
  (size-mul (compute-size (vec-type x))
            (vec-size x)))

(defn compute-record-size [x]
  (reduce size-add (map (comp compute-size :value-type)
                        (:fields x))))

(defn compute-size [type]
  (let [h (access/get type type-head)
        b (access/get type type-body)]
    (cond
      (= :constant h) 0
      (contains? primitive-values h) 1
      (= :vec h) (compute-vec-size b)
      (= :record h) (compute-record-size b)
      :default nil)))

(def parse-type #(spec/conform ::type %))

(assert (= ::spec/invalid (parse-type '(+ 3 4))))

(def compute-size-on (comp compute-size parse-type))

;; 
(assert (= 1 (compute-size-on 9)))
(assert (= 0 (compute-size-on [:constant 9])))
(assert (= 3 (compute-size-on [:vecdata 3 4 5])))
(assert (= 5 (compute-size-on [:vec :double 5])))
(assert (= 5 (compute-size-on [:record :a [:vecdata 1 2 3] :b :double :c :int])))
(assert (nil? (compute-size-on :dynamic)))
(assert (spec/valid? ::type 9))
(assert (= [:long [:value 9]] (spec/conform ::type 9)))
(assert (spec/valid? ::type [:record :a [:constant 9] :b :double]))

(declare visit-primitives)

(defn visit-vec-primitives [b f]
  (if (= (:type b) :vecdata)
    (update-in b [:data] 
               (fn [fields]
                 (map #(visit-primitives % f) fields)))
    (update-in b [:value-type] #(visit-primitives % f))))

(defn visit-record-primitives [b f]
  (update-in b [:fields]
             (fn [fields]
               (map (fn [field]
                      (update-in field [:value-type] 
                                 #(visit-primitives % f)))
                    fields))))

(defn visit-primitives [type f] 
  (let [h (access/get type type-head)]
    (cond
      (contains? primitive-values h) (f type)
      (= :vec h) (access/update type type-body
                                 #(visit-vec-primitives % f))
      (= :record h) (access/update type type-body 
                                 #(visit-record-primitives % f))
      :default type)))

(defn visit-primitive-on [x f]
  (visit-primitives (parse-type x) f))

(assert (= [:visited [:long [:value 9]]]
           (visit-primitive-on 9 (fn [x] [:visited x]))))
(assert (= [:vec {:type :vecdata, :data '([:visited [:long [:value 1]]] 
                                          [:visited [:long [:value 2]]] 
                                          [:visited [:long [:value 3]]])}]
           (visit-primitive-on [:vecdata 1 2 3] (fn [x] [:visited x]))))
(assert (= (visit-primitive-on [:record :a 1 :b 2 :c 3] (fn [x] [:visited x]))
           [:record {:type :record, 
                     :fields '({:key :a, :value-type [:visited [:long [:value 1]]]} 
                               {:key :b, :value-type [:visited [:long [:value 2]]]} 
                               {:key :c, :value-type [:visited [:long [:value 3]]]})}]))

(defn strip-data [x]
  (visit-primitives 
   x (fn [primitive]
       (let [t (access/get primitive type-head)]
         [t [:type t]]))))
(assert (= (strip-data (spec/conform ::type [:vecdata 1 2 3 4]))
           [:vec {:type :vecdata, 
                  :data '([:long [:type :long]] 
                          [:long [:type :long]] 
                          [:long [:type :long]] 
                          [:long [:type :long]])}]))

(def get-type-head (:checked-get type-head))
(def get-type-body (:checked-get type-body))



    
;(defmultiple export-typed-value first
;  (:primitive [x] (export-primitive x)))
;; (defn export-typed-value [x]
;;   (let [h (get-type-head x)
;;         b (get-type-body x)]
;;     (cond 
;;       (contains? primitive-values h) (get-primitive-value b)
;;       (= h :record) (export-record b)
;;       (= h :vec) (export-vec b)
;;       :default x)))
    

                       

;; How to specify one:
;; (value [:record :a :number :b double] [3 [4 9]])


(assert (spec/valid? ::number :number))
(assert (spec/valid? ::record [:record 
                               :a :number 
                               :b [:vec nil 3]]))


(def pair-settings {:default-base [nil nil]})

(def pair-tag (access/index-accessor 0 pair-settings))
(def pair-value (access/index-accessor 1 pair-settings))

(def get-pair-tag (:checked-get pair-tag))
(def get-pair-value (:checked-get pair-value))

;;;;;; Type properties
;; When it is stored

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Grammar
(spec/def ::arg-name symbol?)

(spec/def ::arg-decl (spec/cat
                    :type ::type
                    :name ::arg-name))

(spec/def ::arg-list (spec/* ::arg-decl))

(spec/def ::sexpr (spec/cat 
                   :op ::expr
                   :args (spec/* ::expr)))

(spec/def ::expr (spec/or :number number?
                          :keyword keyword?
                          :symbol symbol?
                          :sexprs ::sexpr
                          :string string?))
(spec/def ::exprs (spec/* ::expr))

(spec/def ::typed-fun (spec/cat
                       :return-type ::type
                       :arg-list (spec/spec ::arg-list)
                       :body ::exprs))
(assert (not= 
         ::spec/invalid 
         (spec/conform ::typed-fun [nil [:double 'a :double 'b] :a :b :c])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Compilable: Everything is here!!!

(defn expr-access [i]
  (access/index-accessor i {:default-base [nil nil]}))
(def expr-type (expr-access 0))
(def expr-value (expr-access 1))



(defn expr-to-typed-value [expr]
  (spec/conform ::type expr))

;;; OBS: Vi måste räkna referenserna till varje uttryck som vi kompilerar.
;;; Kanske i argmap? Om antalet referenser överskriver 1 binder vi uttrycket
;;; till en variabel.
(defmultiple compile-expr (fn [argmap expr cb]
                            (access/get type expr-type))
  (:default 
   [argmap expr cb]
   (cb argmap (expr-to-typed-value (access/get expr expr-value)))))

(def empty-argmap {})


(defn value? [x]
  true)

(defn check-is-value [x]
  (if (not (value? x))
    (throw (RuntimeException. 
            (str "Invalid value: " x)))))

(defn compile-exprs [argmap exprs cb]
  (cond
    (empty? exprs) nil
    (empty? (rest exprs)) (compile-expr argmap (first exprs) cb)
    :default (compile-expr 
              argmap (first exprs)
              (fn [new-argmap x]
                (check-is-value x)
                (compile-exprs new-argmap (rest exprs) cb)))))

;(macroexpand '(with-typed 9 4 5 6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; A fresh start
;;
;; Obs: expr is wider than type. It holds a type.
;;
;; parse: typed-code -> compilable
;; compile: argmap x compilable x (argmap x expr -> code) -> code
;; get-type: compilable -> expr

(def compilable-types #{nil :expr :if :while :assign :do})

(spec/def ::compilable-type #(contains? compilable-types %))
(spec/def ::compilable (spec/keys :req-un [::compilable-type]))
(def compilable? (partial spec/valid? ::compilable))
(def compilable-key-settings {:valid-base? compilable? :default-base {:compilable-type nil}})
(def compilable-type (access/key-accessor :compilable-type compilable-key-settings))
(def expr (access/key-accessor :expr compilable-key-settings))

(defn try-parse-type [x]
  (let [p (parse-type x)]
    (if (not= ::spec/invalid p)
      (access/build compilable-type :expr
                    expr p))))

(defn try-parse-s-expr [x]
  (try
    (let [[f & r] x
          k (apply (eval f) 
                   (map parse r))]
      (if (compilable? k) k))
    (catch Throwable _ nil)))


(defn if-statement [& args] {:compilable-type :if})

;(defn dup [x]
;  (let [p (parse x)]
;    p))

(defn parse [x]
  (or (try-parse-type x)
      (try-parse-s-expr x)
      (core/common-error "Failed to parse " x)))
  
(assert (compilable? (parse 9)))
(assert (compilable? (parse '(if-statement 9))))

(defn compile-expr [argmap x cb]
  (cb argmap (access/get x expr)))

(defmultiple compile-typed (fn [argmap x cb]
                             (access/get x compilable-type))
  (:expr [argmap x cb]
         (compile-expr argmap x cb)))

(assert (= [:long [:value 9]]
           (compile-typed {} (parse 9) (fn [_ x] x))))

(defn long-to-dynamic [[v x]]
  (assert (= :value v))
  x)

(defn to-dynamic-expr [x] (:checked-get type-head)
  (:long [x] (long-to-dynamic (access/get x type-body))))
  
(assert (= 9 (to-dynamic-expr [:long [:value 9]])))
