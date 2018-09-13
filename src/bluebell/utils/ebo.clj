(ns bluebell.utils.ebo
  "Example-based overloading"
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.specutils :as specutils]
            [clojure.core :as c]
            [bluebell.utils.core :as utils]
            [clojure.set :as cljset]))

(declare filter-positive)

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



(spec/def ::arg-specs (spec/coll-of ::arg-spec))
(spec/def ::fn fn?)
(spec/def ::overload (spec/keys :req-un [::arg-specs ::fn]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Impl
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    (if (c/and (empty? pos-failures)
               (empty? neg-failures))
      (assoc arg-spec :valid? true)
      (merge arg-spec
             {:valid? false
              :pos-failures pos-failures
              :neg-failures neg-failures}))))

(defn- decorate-desc [arg-spec]
  {:pre [(contains? arg-spec :key)]}
  (merge {:desc (str (:key arg-spec))} arg-spec))


(defn- init-overload-state [overload-sym]
  (utils/check-io
   [:pre [(symbol? overload-sym)]]
   
   {:symbol overload-sym
    :dirty? true
    :samples #{}
    :arg-specs {}
    :overloads {}}))

(defn- mark-dirty [x]
  (assoc x :dirty? true))

(defn- unmark-dirty [x]
  (assoc x :dirty? false))

(defn- add-arg-spec [state arg-spec]
  (utils/check-io
   [:pre [(map? state)
          ::arg-spec arg-spec]]
   
   (-> state
       mark-dirty
       (update :samples (partial reduce into) [(:pos arg-spec)
                                               (:neg arg-spec)])
       (assoc-in [:arg-specs (:key arg-spec)] arg-spec))))

(defn- add-arg-specs [state arg-specs]
  (reduce add-arg-spec state arg-specs))

(defn- add-overload [state overload]
  (utils/check-io
   [:pre [(map? state)
          ::overload overload]]

   (let [arg-specs (:arg-specs overload)]
     (-> state
         mark-dirty
         (add-arg-specs arg-specs)
         (assoc-in [:overloads (count arg-specs)
                    (mapv :key arg-specs)]
                   (:fn overload))))))

(defn- rebuild-arg-spec-samples [state]
  (let [samples (:samples state)]
    (update state
            :arg-specs
            (fn [arg-specs]
              (into {}
                    (map (fn [[k v]]
                           [k (assoc
                               v :samples
                               (set
                                (filter-positive v samples)))])
                         arg-specs))))))

(defn cart-prod [a b]
  (transduce
   (comp (map (fn [a] (mapv (fn [b] [a b]) b)))
         cat)
   conj
   []
   a))

(defn compare-sets [a b]
  (cond
    (cljset/subset? a b) :subset
    (cljset/superset? a b) :superset
    (= a b) :equal
    :default :disjoint))

(defn- rebuild-arg-spec-comparisons [state]
  (assert (false? (:dirty? state)))
  (let [arg-specs (:arg-specs state)]
    (assert (map? arg-specs))
    (assoc
     state
     :arg-spec-comparisons
     (into {}
           (map (fn [[[ak av] [bk bv]]]
                  (assert (contains? av :samples))
                  (assert (contains? bv :samples))
                  [[ak bk] (compare-sets (:samples av)
                                         (:samples bv))])
                (cart-prod arg-specs arg-specs))))))

(defn- get-all-overload-arg-lists [state]
  (transduce
   (comp (map (fn [[arity overloads-per-arity]]
                {:pre [(number? arity)
                       (map? overloads-per-arity)]}
                (mapv first overloads-per-arity)))
         cat)
   conj
   []
   (:overloads state)))

(defn- compare-arg-lists [state a b]
  true)

(defn- compute-overload-dominates? [state]
  (let [arg-lists (get-all-overload-arg-lists state)]
    (transduce
     (map (fn [[arg-list-a arg-list-b]]
            [[arg-list-a arg-list-b]
             (compare-arg-lists
              state
              arg-list-a
              arg-list-b)]))
     conj
     {}
     (cart-prod arg-lists arg-lists))))

(defn- rebuild-overload-dominates? [state]
  (assoc
   state
   :overload-dominates?
   (compute-overload-dominates? state)))

(defn- rebuild-all [state]
  (-> state
      rebuild-arg-spec-samples
      rebuild-arg-spec-comparisons
      unmark-dirty))


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
  (when (c/not (:valid? arg-spec))
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

(def arg-spec? (specutils/pred ::arg-spec))

(defn filter-positive [arg-spec samples]
  (utils/check-io
   [:pre [::arg-spec arg-spec
          (coll? samples)]
    :post k [(coll? k)]]
   (filter (:pred arg-spec) samples)))


;;;------- Overload -------



;;;------- Common arg specs -------
