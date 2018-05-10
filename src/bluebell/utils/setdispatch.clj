(ns bluebell.utils.setdispatch
  (:require [bluebell.utils.symset :as ss]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.core :as utils]
            [bluebell.utils.specutils :as sutils])
  (:refer-clojure :exclude [complement any?]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Flag used to check that we don't add extra definitions
;; after having called dispatch functions
(def empty-dispatch-state {:was-called? false})



(defn mark-called [system]
  (let [sys (deref system)]
    (if (:was-called? sys)
      sys
      (swap! system #(assoc % :was-called? true)))))

(defn check-not-used [system]
  (utils/data-assert (not (:was-called? system))
                     "You cannot extend the system *after* starting to use it"
                     {:system system})
  system)

;;;; OBS: För ett givet system måste alla 


(spec/def ::fn fn?)
(spec/def ::match-fn fn?)
(spec/def ::feature-extractor fn?)

(spec/def ::arity integer?)
(spec/def ::fn-info (spec/keys :req-un [::fn ::match-fn]))
(spec/def ::fns-per-arty (spec/map-of integer? ::fn-info))
(spec/def ::dispatch-map (spec/map-of ::arity ::fns-per-arity))
(spec/def ::feature-extractor fn?)
(spec/def ::dispatch-state (spec/keys :req-un [::dispatch-map ::feature-extractor]))

(def cljany? clojure.core/any?)
(spec/def ::arg (spec/spec (spec/cat :feature-extractor (spec/? cljany?)
                                     :query cljany?
                                     :binding cljany?)))

(spec/def ::method-meta #(or (map? %)
                             (string? %)
                             (symbol? %)))

(def not-empty? (comp not empty?))

(spec/def ::method-args (spec/cat :name symbol?
                                  :meta (spec/* ::method-meta)
                                  :args (spec/and (spec/coll-of ::arg)
                                                  not-empty?)
                                  :body (spec/* cljany?)))


(defn clean-alt [alt]
  (select-keys alt [:meta :args :satisfied? :generality]))

(defn clean-alts [alts]
  (map clean-alt alts))

(defn match-error-map [arity alts]
  {:arity arity
   :alternatives (sort-by :generality (clean-alts alts))})

(defn resolve-fn-call [system dispatch-state args]
  (let [arity (count args)
        alternatives (map (fn [[k alt]]
                            (merge alt ((:match-fn alt)
                                        system
                                        (:feature-extractor dispatch-state)
                                        args)))
                          (get-in dispatch-state [:dispatch-map arity]))
        matching-alternatives (sort-by :generality (filter :satisfied? alternatives))]

    (println "The alternatives are " (clean-alts matching-alternatives))
    
    (cond
      (empty? matching-alternatives) (throw (ex-info "No matching set-fn for this arity."
                                                     (match-error-map
                                                      arity
                                                      alternatives)))
      (= (:generality (first matching-alternatives))
         (:generality (second matching-alternatives))) (throw
                                                        (ex-info
                                                         "Ambiguous set-based dispatch"
                                                         (match-error-map
                                                          arity
                                                          matching-alternatives)))
      :default (apply (-> matching-alternatives
                          first
                          :fn)
                      args))))

(defn dispatch-root [system dispatch-state]
  (fn [& args]
    (let [system (mark-called system)]
      (if (empty? args)
        dispatch-state
        (resolve-fn-call system
                         (deref dispatch-state)
                         args)))))

(defn initialize-dispatch-state [feature-extractor]
  {:dispatch-map {}
   :feature-extractor feature-extractor})

(defn forward-set-fn [f]
  (fn [& args]
    (let [[system & rest-args] args]
      (swap! system (fn [set-registry]
                      (check-not-used set-registry)
                      (apply f `(~set-registry ~@rest-args)))))))

(defn add-method [state-atom arity method]
  (swap! state-atom
         (fn [state]
           (update-in state [:dispatch-map arity]
                      #(conj (or % {}) [(:args method) method])))))

(def memoized-evaluate-query (memoize ss/evaluate-query))

(defn evaluate-arg-match [system common-feature-extractor arg-spec arg]
  (let [fe (or (:feature-extractor arg-spec)
               common-feature-extractor)
        element (fe arg)

        _ (utils/data-assert (ss/element? system element)
                             "Not an element"
                             {:x element
                              :elements (ss/all-elements system)})
        
        raw-query (:query arg-spec)
        query (ss/normalize-query raw-query)

        ;; TODO: This can be cached
        elements (memoized-evaluate-query system query)

        ;; NOTE: A a query can be satisfied even if the elements returned by
        ;; evaluate-query is empty.
        satisfied? (ss/satisfies-query? system query element)
        
        generality (count elements)]
    (utils/map-of element satisfied? generality raw-query)))

(defn make-match-fn [meta arg-specs]
  (fn [system common-feature-extractor args]
    (assert (= (count args)
               (count arg-specs)))
    (let [evaluated (map (fn [arg-spec arg]
                           (evaluate-arg-match system common-feature-extractor arg-spec arg))
                         arg-specs
                         args)
          all-satisfied? (every? :satisfied? evaluated)
          generality (if all-satisfied?
                       (transduce (map :generality)
                                  +
                                  0
                                  evaluated))]
      {:meta meta
       :args evaluated
       :satisfied? all-satisfied?
       :generality generality})))

(defn subset-of-sub [system a b]
  (-> system
      (ss/add a)
      (ss/add b)
      (ss/subset-of a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  High level API
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use this function to register a type in the system
(def add (forward-set-fn ss/add))

;; Use this function to specify that some set is a subset of another set,
;; e.g. floating point numbers are a subset of numbers
(defn subset-of [system-atom a b]
  (swap! system-atom
         (fn [system]
           (check-not-used system)
           (subset-of-sub system a b))))

;; Query API: Use these functions to build complex set queries
;; for the function arguments
(def universe ss/universe)
(def complement ss/complement)
(def union ss/union)
(def intersection ss/intersection)
(def any? ss/any?)
(def difference ss/difference)


;; This creates a mutable type system onto which we can register
;; new types
(defmacro def-system [system-name]
  `(def ~system-name (atom (merge ss/empty-set-registry
                                  empty-dispatch-state))))

;; This defines the root function that will do the actual dispatch.
(defmacro def-dispatch [fn-name system feature-extractor?]
  (assert (symbol? fn-name))
  `(let [state# (atom (initialize-dispatch-state ~feature-extractor?))]
     (def ~fn-name (dispatch-root ~system state#))))


;; This defines a specific implementation that will be selected if the
;; arguments match the arg spec specifically enough. The more specific the arguments,
;; the more likely this implementation is to be selected.
;;
;; NOTE: This form does not necessarily have to be top-level, it can also
;; be generated.
(defmacro def-set-method [& args0]
  (let [parsed (sutils/force-conform ::method-args args0)
        args (:args parsed)
        arity (count args)]
    `(let [state-atom# (~(:name parsed))]
       (add-method state-atom#
                   ~arity
                   {:args (quote ~args)
                    :match-fn (make-match-fn ~(:meta parsed)
                                             ~(mapv (fn [x] (dissoc x :binding))
                                                    args))
                    :fn (fn [~@(map :binding args)]
                          ~@(:body parsed))}))))

#_ (defmacro def-) ;;; TODO: For every argument a function that produces an element...
