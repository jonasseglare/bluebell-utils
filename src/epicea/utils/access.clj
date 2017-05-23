(ns epicea.utils.access
  (:require [epicea.utils.debug :as debug]
            [epicea.utils.core :as core]
            [epicea.utils.optional :refer [optional]]
            [clojure.spec :as spec]
            [clojure.pprint :as pprint]))

(defn shorten [x]
  (let [n 100]
    (if (< (count x) n)
      x
      (str (subs x 0 n) "..."))))

(defn data-to-str [x]
  (shorten (with-out-str (pprint/pprint x))))

(defn accessor-error [what accessor root value]
  (core/common-error 
   (str "ACCESSOR ERROR"
        "\n\n  * In the context '" what "'"
        "\n\n  * With accessor\n" (data-to-str accessor)
        "\n\n  * On root value  " (data-to-str root)
        "\n\n  * For some value " (data-to-str value))))

(def base-opts {:info nil
                :valid-base? (constantly true)
                :default-base {}
                :default-value nil
                :valid-value? (constantly true)})

(defn map-methods [key] ;;; Every object supports get, set and remove.
  {:get #(get % key) ;; get
   :can-get? #(contains? % key)
   :set (fn [obj x] (assoc obj key x)) ;; set
   :can-set? (constantly true)
   :remove #(dissoc % key) ;; remove
   :can-remove? (constantly true)})

(defn default-map-opts [key]
  {:valid-base? map?
   :info {:type :key-accessor :key key}})

(defn apply-decorator [accessor [key dec]]
  (if (contains? accessor key)
    accessor
    (assoc accessor key (dec accessor))))


(defn make-validate-base [accessor]
  (let [v? (:valid-base? accessor)]
    (fn [x]
      (if (v? x) 
        x 
        (accessor-error "validate-base" 
                        accessor x nil)))))

(defn make-validate-has [accessor]
  (let [h? (:can-get? accessor)]
    (fn [x]
      (if (h? x)
        x
        (accessor-error "missing value"
                        accessor x nil)))))

(defn make-valid-output? [accessor]
  (:valid-value? accessor))

(defn make-valid-input? [accessor]
  (let [c? (:can-set accessor)
        v? (:valid-value? accessor)]
    (fn [obj x]
      (and (v? x)
           (c? obj x)))))

(defn make-validate-value [accessor]
  (let [v? (:valid-output? accessor)]
    (fn [x]
      (if (v? x)
        x
        (accessor-error "invalid value"
                        accessor x nil)))))

(defn make-get-optional-unchecked [accessor]
  (let [h? (:can-get? accessor)
        g (:get accessor)]
    (fn [x]
      (if (h? x)
        (optional (g x))
        (optional)))))

(defn make-get-optional [accessor]
  (let [b (:validate-base accessor)
        v (:validate-value accessor)
        o (:get-optional-unchecked accessor)]
    (fn [x]
      (if-let [[y] (o (b x))]
        (optional (v y))
        (optional)))))

(defn make-checked-get [accessor]
  (let [b (:validate-base accessor)
        h (:validate-has accessor)
        v (:validate-value accessor)
        g (:get accessor)]
    (comp v g h b)))

(defn make-checked-set [accessor]
  (let [b (:validate-base accessor)
        v (:validate-value accessor)
        s (:set accessor)]
    (fn [obj x]
      (b (s (b obj) (v x))))))

(defn make-checked-remove [accessor]
  (let [b (:validate-base accessor)
        r (:remove accessor)
        r? (:can-remove? accessor)]
    (fn [obj0]
      (let [obj (b obj0)]
        (if (not (r? obj)) 
          (accessor-error "Cannot remove" accessor obj nil)
          (r obj))))))
    

(defn make-update [accessor]
  (let [g (:checked-get accessor)
        s (:checked-set accessor)]
    (fn [obj f]
      (s obj (f (g obj))))))

(defn make-prepare [accessor]
  (let [h? (:can-get? accessor)
        d (:default-value accessor)
        s (:set accessor)]
    (fn [obj]
      (if (h? obj)
        obj
        (s obj d)))))

(defn make-get-or-default [a]
  (let [b (:validate-base a)
        p (:prepare a)
        h (:validate-has a)
        g (:get a)
        v (:validate-value a)]
    (comp v g h p b)))

(def decorators [[:validate-base make-validate-base]
                 [:validate-has make-validate-has]
                 [:valid-output? make-valid-output?]
                 [:valid-input? make-valid-input?]
                 [:validate-value make-validate-value]
                 [:get-optional-unchecked make-get-optional-unchecked]
                 [:get-optional make-get-optional]
                 [:checked-get make-checked-get]
                 [:checked-set make-checked-set]
                 [:checked-remove make-checked-remove]
                 [:update make-update]
                 [:prepare make-prepare]
                 [:get-or-default make-get-or-default]])

(defn decorate-accessor [a]
  (reduce apply-decorator
          a
          decorators))

(defn key-accessor 
  ([key extra-opts]
   (decorate-accessor
    (merge base-opts 
           (default-map-opts key)
           extra-opts 
           (map-methods key))))
  ([key] (key-accessor key {})))

;;;;;; Main map creator
(def q (key-accessor :q))

(defn build-accessor [default-opts user-opts methods]
  (decorate-accessor
   (merge base-opts
          default-opts
          user-opts
          methods)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Vector
(defn default-vector-opts [index]
  {:info {:type :index-accessor :index index}}
  {:valid-base? vector?})

(defn vector-methods [index]
  {:can-get? #(< index (count %))
   :get #(nth % index)
   :set (fn [obj x] (assoc obj index x))
   :can-remove? (constantly false)
   :remove identity})

(defn index-accessor 
  ([index extra-opts] 
   (decorate-accessor 
    (merge base-opts
           (default-vector-opts index)
           extra-opts
           (vector-methods index))))
  ([index] (index-accessor index {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Composite map accessor
(defn get-required [m]
  (filter 
   (fn [[_ value]]
     (not (vector? value)))
   m))

(defn get-optional [m]
  (filter
   identity
   (map
    (fn [[key value]]
      (if (vector? value)
        [key (first value)]))
    m)))
   
(defn default-composite-map-opts [m]
  {:info {:type :map-accessor :map m}
   :valid-base? map?})

(defn composite-map-methods [m]
  nil)

(defn map-accessor [m extra-opts]
  (build-accessor
   (default-composite-map-opts m)
   extra-opts
   (composite-map-methods m)))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Composition: TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Convenient macros: TODO
