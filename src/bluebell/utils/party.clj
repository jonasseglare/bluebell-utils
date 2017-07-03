(ns bluebell.utils.party
  (:refer-clojure :exclude [update])
  (:require [clojure.spec :as spec]
            [bluebell.utils.defmultiple :refer [defmultiple]]
            [bluebell.utils.core :as utils]))

(defn key-accessor
  "Create an accessor for map keys"
  [k]
  (fn
    ([] {:desc (str "(key-accessor " k ")")})
    ([obj]
     (get obj k))
    ([obj x]
     (assoc obj k x))))

(defn index-accessor
  "Create an accessor for indices"
  [i]
  (fn
    ([] {:desc "(index-accessor " i ")"})
    ([obj] (nth obj i))
    ([obj x] (assoc obj i x))))

(defn chain2 [a b]
  (fn
    ([] {:desc "(chain2 " (:desc (a)) " " (:desc (b)) ")"})
    ([obj] (if-let [aobj (a obj)]
             (b aobj)))
    ([obj x] (a obj (b (a obj) x)))))

(defn chain [& args]
  "Connect accessors in a chain"
  (reduce chain2 args))

(defn update [obj accessor f]
  "Use an accessor to update an object"
  (accessor obj (f (accessor obj))))

;;;;;;;;;;;;;;;;;;;;;; Validation
(defn validate [v? x cmt]
  (assert (v? x) cmt)
  x)

(defn validate-base [accessor v?]
  (let [cmt (str "(validate-base " (:desc (accessor)) " v?)")]
    (fn
      ([] {:desc cmt})
      ([obj] (accessor (validate v? obj cmt)))
      ([obj x] (accessor (validate v? obj cmt) x)))))

(defn validate-target [accessor v?]
  (let [cmt (str "(validate-target " (:desc (accessor)) " v?)")]
    (fn
      ([] {:desc cmt})
      ([obj] (validate v? (accessor obj) cmt))
      ([obj x] (accessor obj (validate v? x cmt))))))

;;;;;;;;;;;;;;;;;;;;; pseudo record

(spec/def ::opts map?)

(spec/def ::field (spec/alt :name symbol?
                            :detailed (spec/cat :name symbol?
                                                :opts map?)))

(spec/def ::args (spec/cat :name symbol?
                           :opts (spec/? map?)
                           :fields (spec/* ::field)))

(defn default-opts [n]
  {:name n
   :key (keyword n)
   :valid? nil})

(defmultiple homogenize-field first
  (:detailed [[_ {name :name opts :opts}]]
             (merge (default-opts name) opts))
  (:name [[_ name]] (default-opts name)))

(defn get-fields [parsed]
  (map homogenize-field (:fields parsed)))

(defn rec-validator [fields0]
  (let [fields (set fields0)]
    (fn [x]
      (and (map? x)
           (clojure.set/subset? fields (keys x))))))

(defn default-rec-opts [fields]
  {:valid? `(rec-validator ~fields)})

(defn defpseudorec-sub [args]
  (let [fields (get-fields args)
        id (:name args)]
    `(do
       (def ~id ~(merge (default-rec-opts fields)
                        (or (:opts args) {})))
       (def ~(symbol (str (name id) "?"))
         (:valid? ~id)))
    ))

(defmacro defpseudorec [& args]
  (let [parsed (spec/conform ::args args)]
    (if (= ::spec/invalid parsed)
      (utils/common-error "defpseudorec failed: " (spec/explain-str ::args args))
      (defpseudorec-sub parsed))))
