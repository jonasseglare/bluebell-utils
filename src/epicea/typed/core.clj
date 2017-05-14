(ns epicea.typed.core
  (:require [clojure.spec :as spec]
            [epicea.utils.debug :as debug]
            [epicea.utils.defmultiple :refer [defmultiple]]
            [epicea.utils.access :as access] :reload-all))

(spec/def ::unspecified nil?)

(spec/def ::dynamic (partial = :dynamic))

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
                          :dynamic symbol?))

(def pair-vec {:default-parent [nil nil]})
(def type-head (access/vector-accessor 0 pair-vec))
(def type-body (access/vector-accessor 1 pair-vec))

(def primitive-values #{:double :float :long :int :bool :number})

(declare compute-size)

(defn size-op [f]
  (fn [a b]
    (if (and (number? a) (number? b))
      (f a b))))
(def size-add +)
(def size-mul *)
(def size-max max)

(defn compute-vec-size [x]
  (size-mul (compute-size (vec-type x))
            (vec-size x)))

(defn compute-record-size [x]
  (reduce size-add (map (comp compute-size :value-type)
                        (:fields x))))

(defn compute-size [type]
  (let [h (access/getx type-head type)
        b (access/getx type-body type)]
    (cond
      (= :constant h) 0
      (contains? primitive-values h) 1
      (= :vec h) (compute-vec-size b)
      (= :record h) (compute-record-size b)
      :default nil)))

(def compute-size-on (comp compute-size #(spec/conform ::type %)))

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

(defn export-primitive [x]
  [:export x])


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
    

;(defmultiple export-typed-value first
;  (:primitive [x] (export-primitive x)))
(defn export-typed-value [x]
  [:export x])
                       

;; How to specify one:
;; (value [:record :a :number :b double] [3 [4 9]])


(assert (spec/valid? ::number :number))
(assert (spec/valid? ::record [:record 
                               :a :number 
                               :b [:vec nil 3]]))


(def pair-settings {:default-parent [nil nil]})

(def pair-tag (access/vector-accessor 0 pair-settings))
(def pair-value (access/vector-accessor 1 pair-settings))

(def get-pair-tag (access/getter pair-tag))
(def get-pair-value (access/getter pair-value))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Compilable

(defn expr-access [i]
  (access/vector-accessor i {:default-parent [nil nil]}))
(def expr-type (expr-access 0))
(def expr-value (expr-access 1))



(defn expr-to-typed-value [expr]
  (spec/conform ::type expr))

;;; OBS: Vi måste räkna referenserna till varje uttryck som vi kompilerar.
;;; Kanske i argmap? Om antalet referenser överskriver 1 binder vi uttrycket
;;; till en variabel.
(defmultiple compile-expr (fn [argmap expr cb]
                            (access/getx expr-type expr))
  (:default 
   [argmap expr cb]
   (cb argmap (expr-to-typed-value (access/getx expr-value expr)))))

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

(defmacro with-typed [& args]
  (let [parsed (spec/conform ::exprs args)]
    (if (= ::spec/invalid parsed)
      (throw (RuntimeException. 
              (str "Failed to parsed typed: " 
                   (spec/explain-str ::exprs args))))
      (compile-exprs empty-argmap parsed (fn [final-argmap final-value]
                                           (check-is-value final-value)
                                           (export-typed-value final-value))))))
  
  
(macroexpand '(with-typed 9 4 5 6))
