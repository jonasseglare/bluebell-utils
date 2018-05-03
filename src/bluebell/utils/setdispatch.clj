(ns bluebell.utils.setdispatch
  (:require [bluebell.utils.symset :as ss]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.specutils :as sutils]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Flag used to check that we don't add extra definitions
;; after having called dispatch functions
(def empty-dispatch-state {:was-called? false})



(defn mark-called [system]
  (if (not (:was-called? (deref system)))
    (swap! system #(assoc % :was-called? true))))

;;;; OBS: För ett givet system måste alla 

(defn dispatch-root [system fn-list]
  (fn [& args]
    (mark-called system)
    (if (empty? args)
      fn-list
      )))


(spec/def ::fn fn?)
(spec/def ::match-fn fn?)
(spec/def ::feature-extractor fn?)

(spec/def ::fn-info (spec/keys :req-un [::fn ::match-fn]))
(spec/def ::fns-per-arty (spec/map-of integer? ::fn-info))
(spec/def ::dispatch-map (spec/map-of ::arity ::fns-per-arity))
(spec/def ::feature-extractor fn?)
(spec/def ::dispatch-state (spec/keys :req-un [::dispatch-map ::feature-extractor]))



(spec/def ::arg (spec/spec (spec/cat :get-feature (spec/? any?)
                                     :set any?
                                     :binding any?)))

(spec/def ::method-args (spec/cat :name symbol?
                                  :args (spec/coll-of ::arg)
                                  :body (spec/* any?)))

(defn initialize-dispatch-state [feature-extractor]
  {:dispatch-map {}
   :feature-extractor feature-extractor})

(defn forward-set-fn [f]
  (fn [& args]
    (let [[system & rest-args] args]
      (swap! system (fn [set-registry]
                      (apply f `(~set-registry ~@rest-args)))))))

(defn add-method [state-atom arity method]
  (swap! state-atom
         (fn [state]
           (update-in state [:dispatch-map arity]
                      #(conj % method)))))

(defn make-match-fn [state-atom arglist])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  High level API
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def add (forward-set-fn ss/add))
(def member-of (forward-set-fn ss/member-of))
(def subset-of (forward-set-fn ss/subset-of))


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
    (println "PARSED:" parsed)
    `(let [state-atom# (~(:name parsed))]
       (add-method state-atom#
                   ~(count (:args parsed))
                   {:match-fn (make-match-fn ~@(map (fn [x] (dissoc x :binding))
                                                    (:args parsed)))
                    :fn (fn [~@(map :binding (:args parsed))]
                          ~@(:body parsed))}))))

#_ (defmacro def-) ;;; TODO: For every argument a function that produces an element...
