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
        "\n  In the context " what
        "\n  With accessor  " (data-to-str accessor)
        "\n  On root value  " (data-to-str root)
        "\n  For some value " (data-to-str value))))

(def base-opts {:info nil
                :valid-base? (constantly true)
                :default-base {}
                :default-value nil
                :valid-value? nil})

(defn map-methods [key]
  {:has? #(contains? % key)
   :get #(get % key)
   :set (fn [obj x] (assoc obj key x))})

(defn default-map-opts [key]
  {:valid-base? map?
   :info {:type :map-accessor :key key}})

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

(defn make-validate-value [accessor]
  (let [v? (:valid-value? accessor)]
    (fn [x]
      (if (v? x)
        x
        (accessor-error "invalid value"
                        accessor x nil)))))

(def decorators [[:validate-base make-validate-base]
                 [:validate-has make-validate-has]
                 [:validate-value make-validate-value]])

(defn decorate-accessor [a]
  (reduce apply-decorator
          a
          decorators))

;;;;;; Main map creator
(defn map-accessor 
  ([key extra-opts]
   (decorate-accessor
    (merge base-opts 
           (default-map-opts key)
           extra-opts 
           (map-methods key))))
  ([key] (map-accessor key {})))


(def q (map-accessor :q))
