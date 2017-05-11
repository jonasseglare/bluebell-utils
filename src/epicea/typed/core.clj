(ns epicea.typed.core
  (:require [clojure.spec :as spec]
            [epicea.utils.debug :as debug]
            [epicea.utils.defmultiple :refer [defmultiple]]
            [epicea.utils.access :as access] :reload-all))

(spec/def ::unspecified nil?)
(spec/def ::bool boolean?)
(spec/def ::float float?)
(spec/def ::double double?)
(spec/def ::long #(instance? java.lang.Long %))
(spec/def ::int #(instance? java.lang.Integer %))
(spec/def ::primitive (spec/or
                       :bool (partial = :bool)
                       :float (partial = :float)
                       :long (partial = :long)
                       :double (partial = :double)
                       :int (partial = :int)))
(spec/def ::dynamic (partial = :dynamic))

(spec/def ::number (spec/or :type #(or (= % :number)
                                       (= % :double))
                            :literal number?))
(spec/def ::count number?)
(spec/def ::key keyword?)

(spec/def ::record-field (spec/cat 
                          :key ::key
                          :value ::sized-type))

(spec/def ::record (spec/cat
                    :type (partial = :record)
                    :fields (spec/* ::record-field)))

(spec/def ::union (spec/cat
                   :type (partial = :union)
                   :alts (spec/* ::sized-type)))

(spec/def ::vec (spec/cat
                 :type (partial = :vec)
                 :type ::sized-type
                 :count ::count))

(spec/def ::vecdata (spec/cat
                     :type (partial = :vecdata)
                     :data (spec/* ::sized-type)))

;; Compile time tagging
(spec/def ::tagged (spec/cat
                    :type (partial = :tag)
                    :tag (constantly true)
                    :data ::sized-type))

(spec/def ::sized-type (spec/or :number ::number
                                :record ::record
                                :union ::union
                                :unspecified ::unspecified ;; <-- a primitive building block
                                :tagged ::tagged
                                :vec ::vec
                                :vecdata ::vecdata))

(spec/def ::array (spec/cat
                   :type (partial = :array)
                   :header (spec/? ::sized-type)
                   :data ::sized-type))

(spec/def ::type (spec/or :sized-type ::sized-type
                          :primitive ::primitive
                          :array ::array
                          :dynamic ::dynamic))
                       

;; How to specify one:
;; (value [:record :a :number :b double] [3 [4 9]])

(assert (spec/valid? ::number :number))
(assert (spec/valid? ::union [:union :number :number]))
(assert (spec/valid? ::record [:record 
                               :a :number 
                               :b [:vec nil 3]]))
(assert (spec/valid? ::tagged [:tag 119 [:vec :number 3]]))
(assert (spec/valid? ::array [:array [:record :a nil :b nil]]))
(assert (spec/valid? ::array [:array [:record :a nil :b nil] nil]))
(assert (spec/valid? ::vecdata [:vecdata 3 4 5]))

(def pair-settings {:default-parent [nil nil]})

(def pair-tag (access/vector-accessor 0 pair-settings))
(def pair-value (access/vector-accessor 1 pair-settings))

(def get-pair-tag (access/getter pair-tag))
(def get-pair-value (access/getter pair-value))

;;;;;; Type properties
(declare compute-size)

(defn size-op [f]
  (fn [a b]
    (if (and (number? a) (number? b))
      (f a b))))
(def size-add +)
(def size-mul *)
(def size-max max)


;;;; Compute the size of a ::sized-type
(defmultiple compute-size get-pair-tag
  (:vec [x] (let [value (get-pair-value x)]
              (size-mul (:count value) (compute-size (:type value)))))
  (:number [x] 1)
  (:tagged [x] (compute-size (:data (get-pair-value x))))
  (:union [x] (reduce size-max (map compute-size (-> x get-pair-value :alts))))
  (:record [x] (reduce size-add (map (comp compute-size :value) 
                                    (-> x get-pair-value :fields))))
  (:unspecified [x] nil)
  (:vecdata [x] (reduce size-add (map compute-size (-> x get-pair-value :data)))))

  
(assert (= 1 (compute-size (spec/conform ::sized-type [:tag 119 :number]))))
(assert (= 1 (compute-size (spec/conform ::sized-type 3))))
(assert (= 3 (compute-size (spec/conform ::sized-type [:vec :number 3]))))
(assert (= 2 (compute-size (spec/conform ::sized-type [:union :number [:vec :number 2]]))))
(assert (= 2 (compute-size (spec/conform ::sized-type [:record :a :number :b :number]))))
(assert (= 3 (compute-size (spec/conform 
                            ::sized-type 
                            [:record :a :number :b :number :c :number]))))

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

(defn compile-expr [argmap expr cb]
  (cb argmap [:compiled expr]))

(def empty-argmap {})

(defn compile-exprs [argmap exprs cb]
  (cond
    (empty? exprs) nil
    (empty? (rest exprs)) (compile-expr argmap (first exprs) cb)
    :default (compile-expr 
              argmap (first exprs)
              (fn [new-argmap _]
                (compile-exprs new-argmap (rest exprs) cb)))))

(defmacro with-typed [& args]
  (let [parsed (spec/conform ::exprs args)]
    (if (= ::spec/invalid parsed)
      (throw (RuntimeException. 
              (str "Failed to parsed typed: " 
                   (spec/explain-str ::exprs args))))
      (compile-exprs empty-argmap parsed (fn [final-argmap final-value]
                                           [:final final-value])))))
  
  
(macroexpand '(with-typed 9 (+ 4 5)))
