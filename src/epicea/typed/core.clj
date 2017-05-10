(ns epicea.typed.core
  (:require [clojure.spec :as spec]
            [epicea.utils.debug :as debug]
            [epicea.utils.defmultiple :refer [defmultiple]]
            [epicea.utils.access :as access] :reload-all))

(spec/def ::double (spec/or :type (partial = :double)
                            :literal number?))
(spec/def ::unspecified nil?)
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
                     :values (spec/* ::sized-type)))

;; Compile time tagging
(spec/def ::tagged (spec/cat
                    :type (partial = :tag)
                    :tag (constantly true)
                    :data ::sized-type))

(spec/def ::sized-type (spec/or :double ::double
                                :record ::record
                                :union ::union
                                :unspecified ::unspecified
                                :tagged ::tagged
                                :vec ::vec
                                :vecdata ::vecdata))

(spec/def ::array (spec/cat
                   :type (partial = :array)
                   :header (spec/? ::sized-type)
                   :data ::sized-type))

;; How to specify one:
;; (value [:record :a :double :b double] [3 [4 9]])

(assert (spec/valid? ::double :double))
(assert (spec/valid? ::union [:union :double :double]))
(assert (spec/valid? ::record [:record 
                               :a :double 
                               :b [:vec nil 3]]))
(assert (spec/valid? ::tagged [:tag 119 [:vec :double 3]]))
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

(defmultiple compute-size get-pair-tag
  (:vec [x] (let [value (get-pair-value x)]
              (size-mul (:count value) (compute-size (:type value)))))
  (:double [x] 1)
  (:tagged [x] (compute-size (:data (get-pair-value x))))
  (:union [x] (reduce size-max (map compute-size (-> x get-pair-value :alts))))
  (:record [x] (reduce size-add (map (comp compute-size :value) 
                                    (-> x get-pair-value :fields)))))
  
(assert (= 1 (compute-size (spec/conform ::sized-type [:tag 119 :double]))))
(assert (= 1 (compute-size (spec/conform ::sized-type 3))))
(assert (= 3 (compute-size (spec/conform ::sized-type [:vec :double 3]))))
(assert (= 2 (compute-size (spec/conform ::sized-type [:union :double [:vec :double 2]]))))
(assert (= 2 (compute-size (spec/conform ::sized-type [:record :a :double :b :double]))))
