(ns bluebell.utils.party
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.defmultiple :refer [defmultiple]]
            [bluebell.utils.core :as utils]))

(defn access-identity
  ([] {:desc "access-idenity"})
  ([obj] obj)
  ([old-val new-val] new-val))

(defn access-coll-as-vec
  ([] {:desc "access-coll-as-vec"})
  ([x] (utils/normalize-coll x))
  ([x new-value] (utils/denormalize-coll x new-value)))

(def default-key-accessor-settings {:req-on-get true
                                    :req-on-assoc false})

(defn key-accessor
  "Create an accessor for map keys"
  ([k] (key-accessor k default-key-accessor-settings))
  ([k settings]
   (let [{req-on-get :req-on-get
          req-on-assoc :req-on-assoc} (merge default-key-accessor-settings
                                             settings)]
     (fn
       ([] {:desc (str "(key-accessor " k ")")})
       ([obj]
        (utils/implies req-on-get (contains? obj k))
        (get obj k))
       ([obj x]
        (utils/implies req-on-assoc (contains? obj k))
        (assoc obj k x))))))

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
    ([obj] (b (a obj)))
    ([obj x] (a obj (b (a obj) x)))))

(defn chain [& args]
  "Connect accessors in a chain"
  (reduce chain2 args))

(defn update [obj accessor f]
  "Use an accessor to update an object"
  (accessor obj (f (accessor obj))))

(defn updater [accessor]
  (fn [obj f]
    (update obj accessor f)))

;;;;;;;;;;;;;;;;;;;;;; Validation
(defn validate [v? x cmt]
  (assert (v? x) (str cmt " on " x))
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
           (clojure.set/subset?
            fields (set (keys x)))))))

(defn default-rec-opts [fields]
  {:valid? `(rec-validator ~(vec (map :key fields)))})

(defn field-place-holders [fields]
  (reduce (fn [m x]
            (assoc m (:key x) nil))
          {} fields))

(defn validate-base-or-nil [acc v?]
  (if (nil? v?)
    acc
    (validate-base acc v?)))

(defn validate-target-or-nil [acc v?]
  (if (nil? v?)
    acc
    (validate-target acc v?)))

(defn key-accessor-with-validation
  [vb? k v?]
  (validate-target-or-nil
   (validate-base-or-nil (key-accessor k) vb?)
   v?))

(defn make-field [valid-base? field]
  `(def ~(:name field) (key-accessor-with-validation
                        ~valid-base?
                        ~(:key field)
                        ~(:valid? field))))

(defn make-fields [valid-base? fields]
  (map #(make-field valid-base? %) fields))

(defn defpseudorec-sub [args]
  (let [fields (get-fields args)
        id (:name args)
        defv (field-place-holders fields)
        base (merge (default-rec-opts fields)
                    (or (:opts args) {}))]
    `(do
       (def ~id ~defv)
       (def ~(symbol (str (name id) "?"))
         ~(:valid? base))
       ~@(make-fields (:valid? base) fields))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; The main macro to build a pseudo record
(defmacro defpseudorec
  "Define a pseudo record with methods to access." [& args]
  (let [parsed (spec/conform ::args args)]
    (if (= ::spec/invalid parsed)
      (utils/common-error "defpseudorec failed: " (spec/explain-str ::args args))
      (defpseudorec-sub parsed))))
