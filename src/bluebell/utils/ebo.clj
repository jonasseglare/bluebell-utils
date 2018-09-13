(ns bluebell.utils.ebo
  "Example-based overloading"
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.specutils :as specutils]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::pos coll?)
(spec/def ::neg coll?)
(spec/def ::pred fn?)
(spec/def ::key any?)
(spec/def ::desc string?)
(spec/def ::spec #(or (spec/spec? %)
                      (keyword? %)))
(spec/def ::valid? boolean?)

(spec/def ::arg-spec (spec/keys :req-un [::pred
                                         ::pos
                                         ::neg
                                         ::key
                                         ::valid?
                                         ::desc]
                                
                                :opt-un [::spec]))

(spec/def ::input-arg-spec
  (spec/keys :opt-un [::pred ::pos ::neg
                      ::key ::desc ::spec]))

(defn- decorate-key-and-pred-from-spec [input-arg-spec]
  (if-let [s (:spec input-arg-spec)]
    (merge {:key [::spec s]
            :pred (specutils/pred s)} input-arg-spec)
    input-arg-spec))

(defn- validate-on-samples [arg-spec]
  (let [pred (:pred arg-spec)
        pos-failures (filter (complement pred)
                             (:pos arg-spec))
        neg-failures (filter pred (:neg arg-spec))]
    (if (and (empty? pos-failures)
             (empty? neg-failures))
      (assoc arg-spec :valid? true)
      (merge arg-spec
             {:valid? false
              :pos-failures pos-failures
              :neg-failures neg-failures}))))

(defn- decorate-desc [arg-spec]
  {:pre [(contains? arg-spec :key)]}
  (merge {:desc (str (:key arg-spec))} arg-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn normalize-arg-spec [input-arg-spec]
  {:pre [(spec/valid? ::input-arg-spec input-arg-spec)]
   :post [(spec/valid? ::arg-spec %)]}
  (-> (merge {:pos [] :neg []} input-arg-spec)
      decorate-key-and-pred-from-spec
      decorate-desc
      validate-on-samples))

(defn check-valid [arg-spec]
  {:pre [(spec/valid? ::arg-spec arg-spec)]}
  (when (not (:valid? arg-spec))
    (throw (ex-info
            (str "Invalid arg-spec '" (:desc arg-spec) "'")
            arg-spec)))
  arg-spec)

(defmacro def-arg-spec [sym value]
  {:pre [(symbol? sym)]}
  `(def ~sym
     (check-valid
      (normalize-arg-spec
       (merge {:key [::def-arg-spec
                     ~(keyword (str *ns*) (name sym))]}
              ~value)))))


;;;------- Common arg specs -------
