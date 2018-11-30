(ns bluebell.utils.wip.access
  (:refer-clojure :exclude [get set update remove])
  (:require [bluebell.utils.wip.debug :as debug]
            [bluebell.utils.wip.core :as core]
            [bluebell.utils.wip.optional :refer [optional]]
            [clojure.spec.alpha :as spec]
            [clojure.pprint :as pprint]))

;;;;;
;; Base methods:
;;  - has?
;;  - remove (can ignore the incoming value if not supported)
;;  - set (can ignore the incoming value if not supported)
;;  - get
;;
;; And also
;;  - valid-value?
;;  - valid-base?
;;  - default 

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
        "\n\n  * Accessor info: " (data-to-str (:info accessor))
        "\n\n  * With accessor\n" (data-to-str accessor)
        "\n\n  * On root value  " (data-to-str root)
        "\n\n  * For some value " (data-to-str value))))

(def base-opts {:info nil
                :valid-base? (constantly true)
                :default-base {}
                :default-value nil
                :valid-value? (constantly true)})

(defn key-methods [key] ;;; Every object supports get, set and remove.
  {:get #(clojure.core/get % key) ;; get
   :has? #(contains? % key)
   :set (fn [obj x] (assoc obj key x)) ;; set
   :remove #(dissoc % key)})


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
  (let [h? (:has? accessor)]
    (fn [x]
      (if (h? x)
        x
        (accessor-error "missing value"
                        accessor x nil)))))

(defn make-valid-output? [accessor]
  (:valid-value? accessor))

(defn make-valid-input? [accessor]
  (let [v? (:valid-value? accessor)]
    (fn [obj x]
      (v? x))))
           

(defn make-validate-value [accessor]
  (let [v? (:valid-output? accessor)]
    (fn [x]
      (if (v? x)
        x
        (accessor-error "invalid value"
                        accessor x nil)))))

(defn make-get-optional-unchecked [accessor]
  (let [h? (:has? accessor)
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
        v (:validate-has-not accessor)]
    (fn [obj]
      (r (b obj)))))
    

(defn make-update [accessor]
  (let [g (:checked-get accessor)
        s (:checked-set accessor)]
    (fn [obj f]
      (s obj (f (g obj))))))

(defn make-prepare [accessor]
  (let [h? (:has? accessor)
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

(defn decorate-accessor 
  ([a decs]
   (reduce apply-decorator
           a
           decs))
  ([a] (decorate-accessor a decorators)))

(defn key-accessor 
  ([key extra-opts]
   (decorate-accessor
    (merge base-opts 
           (default-map-opts key)
           extra-opts 
           (key-methods key))))
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
  {:has? #(< index (count %))
   :get #(nth % index)
   :set (fn [obj x] (assoc obj index x))
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

(defn make-map-can-get? [reqs]
  (fn [x]
    (every? (fn [[key acc]]
              ((:has? acc) x))
            reqs)))

(defn add-key-to-map [x optional?]
  (fn [dst [key acc]]
    (if (or (not optional?) ((:has? acc) x))
      (assoc dst key ((:checked-get acc) x))
      dst)))

(defn make-map-get [reqs opts]
  (fn [x]
    (reduce
     (add-key-to-map x true)
     (reduce 
      (add-key-to-map x false)
      {}
      reqs)
     opts)))

  
(defn map-methods [m]
  (let [reqs (get-required m)
        opts (get-optional m)
        all (reduce into [] [reqs opts])]
    {:has? (make-map-can-get? reqs)
     :get (make-map-get reqs opts)}))

(defn map-accessor 
  ([m extra-opts]
   (build-accessor
    (default-composite-map-opts m)
    extra-opts
    (map-methods m)))
  ([m] (map-accessor m {})))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Serial

(defn default-serial-opts [a b]
  {:info [:serial a b]
   :valid-value? (:valid-value? b)
   :valid-base? (:valid-base? a)
   :default-base (:default-base a)
   :default-value (:default-value b)})

(defn make-serial-can-get? [a b]
  (let [ao (:get-optional a)
        cgb? (:has? b)]
    (fn [x]
      (if-let [[y] (ao x)]
        (cgb? y)))))

(defn make-serial-get [a b]
  (let [ga (:get a)
        gb (:get b)]
    (fn [x]
      (gb (ga x)))))

(defn make-serial-set [a b]
  (let [gda (:get-or-default a)
        sa (:set a)
        sb (:set b)]
    (fn [obj x]
      (sa obj (sb (gda obj) x)))))

(defn make-serial-remove [a b]
  (let [ag (:get a)
        as (:set a)
        rb (:remove b)]
    (fn [obj]
      (as obj (rb (ag obj))))))

(defn make-serial-get-optional [a b]
  (let [goa (:get-optional a)
        hb? (:has? b)
        gb (:checked-get b)]
    (fn [x]
      (if-let [[y] (goa x)]
        (if (hb? y)
          (optional (gb y))
          (optional))
        (optional)))))

(defn serial-methods [a b]
  {:has? (make-serial-can-get? a b)
   :get (make-serial-get a b)
   :set (make-serial-set a b)
   :remove (make-serial-remove a b)
   :get-optional (make-serial-get-optional a b)})

(defn serial-accessor2 
  ([a b extra-opts]
   (build-accessor
    (default-serial-opts a b)
    extra-opts
    (serial-methods a b)))
  ([a b]
   (serial-accessor2 a b {})))

(defn serial [& accessors]
  (reduce serial-accessor2 accessors))

;;;;;;;;;;;;;;;;;;;;;; Helpers  
(defn get [obj accessor]
  ((:checked-get accessor) obj))

(defn set [obj accessor value]
  ((:checked-set accessor) obj value))

(defn has? [obj accessor] 
  ((:has? accessor) obj))

(defn remove [obj accessor]
  ((:remove accessor) obj))

(defn update [obj accessor f]
  ((:update accessor) obj f))
  

(defn apply-to-base [obj [accessor value]]
  (set obj accessor value))

(defn build-sub [base args]
  (reduce ((core/bundle 2) 
           apply-to-base) base args))

(defn build [& args]
  (if (even? (count args))
    (build-sub (:default-base (first args)) args)
    (build-sub (first args) (rest args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Composition: TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Convenient macros: TODO




