(ns epicea.typed.core
  (:require [clojure.spec :as spec] :reload-all))

(spec/def ::double (partial = :double))
(spec/def ::unspecified nil?)
(spec/def ::count number?)
(spec/def ::primitive (spec/or :double ::double))

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

(spec/def ::tagged (spec/cat
                    :type (partial = :tag)
                    :tag (constantly true)
                    :data ::sized-type))

(spec/def ::sized-type (spec/or :primitive ::primitive
                                :record ::record
                                :union ::union
                                :unspecified ::unspecified
                                :tagged ::tagged
                                :vec ::vec))

(spec/def ::array (spec/cat
                   :type (partial = :array)
                   :element-type ::sized-type))

(spec/def ::harray (spec/cat
                    :type (partial = :harray)
                    :header ::sized-type
                    :data ::sized-type))

(assert (spec/valid? ::double :double))
(assert (spec/valid? ::union [:union :double :double]))
(assert (spec/valid? ::record [:record 
                               :a :double 
                               :b [:vec nil 3]]))
(assert (spec/valid? ::tagged [:tag 119 [:vec :double 3]]))
(assert (spec/valid? ::array [:array [:record :a nil :b nil]]))
(assert (spec/valid? ::harray [:harray [:record :a nil :b nil] nil]))
