(ns bluebell.utils.setdispatch
  (:require [bluebell.utils.symset :as ss]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.core :as utils]
            [bluebell.utils.specutils :as sutils])
  (:refer-clojure :exclude [complement]))


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

(spec/def ::fn-info (spec/keys :req-un [::fn ::match-fn]))
(spec/def ::fns-per-arty (spec/map-of integer? ::fn-info))
(spec/def ::dispatch-map (spec/map-of ::arity ::fns-per-arity))
(spec/def ::feature-extractor fn?)
(spec/def ::dispatch-state (spec/keys :req-un [::dispatch-map ::feature-extractor]))


(spec/def ::arg (spec/spec (spec/cat :feature-extractor (spec/? any?)
                                     :query any?
                                     :binding any?)))

(spec/def ::method-meta #(or (map? %)
                             (string? %)
                             (symbol? %)))

(spec/def ::method-args (spec/cat :name symbol?
                                  :meta (spec/* ::method-meta)
                                  :args (spec/coll-of ::arg)
                                  :body (spec/* any?)))


(defn clean-alt [alt]
  (select-keys alt [:meta :args :satisfied? :generality]))

(defn clean-alts [alts]
  (map clean-alt alts))

(defn match-error-map [arity alts]
  {:arity arity
   :alternatives (sort-by :generality (clean-alts alts))})

(defn resolve-fn-call [system dispatch-state args]
  (let [arity (count args)
        alternatives (map (fn [alt]
                            (merge alt ((:match-fn alt)
                                        system
                                        (:feature-extractor dispatch-state)
                                        args)))
                          (get-in dispatch-state [:dispatch-map arity]))
        matching-alternatives (sort-by :generality (filter :satisfied? alternatives))]
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
                      #(conj % method)))))

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

;; Specifying relationships between types, etc.
(def add (forward-set-fn ss/add))


(defn subset-of [system-atom a b]
  (swap! system-atom
         (fn [system]
           (check-not-used system)
           (subset-of-sub system a b))))

;; Query API
(def universe ss/universe)
(def complement ss/complement)
(def union ss/union)
(def intersection ss/intersection)
(def difference ss/difference)


;; The set system used
(defmacro def-system [system-name]
  `(def ~system-name (atom (merge ss/empty-set-registry
                                  empty-dispatch-state))))

;; The method name, the system, and how we get the features from function args.
(defmacro def-dispatch [fn-name system feature-extractor?]
  (assert (symbol? fn-name))
  `(let [state# (atom (initialize-dispatch-state ~feature-extractor?))]
     (def ~fn-name (dispatch-root ~system state#))))

(defmacro def-set-method [& args]
  (let [parsed (sutils/force-conform ::method-args args)]
    `(let [state-atom# (~(:name parsed))]
       (add-method state-atom#
                   ~(count (:args parsed))
                   {:match-fn (make-match-fn ~(:meta parsed)
                                             ~(mapv (fn [x] (dissoc x :binding))
                                                    (:args parsed)))
                    :fn (fn [~@(map :binding (:args parsed))]
                          ~@(:body parsed))}))))

#_ (defmacro def-) ;;; TODO: For every argument a function that produces an element...
