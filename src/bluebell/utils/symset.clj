(ns bluebell.utils.symset
  (:require [clojure.spec.alpha :as spec]))

;; Type of operations:
;;   - Is an element part of a set?
;;   - A set can be a subset of another set
;;   - An element belongs to a number of sets

(spec/def ::set-id keyword?)
(spec/def ::element-id any?)
(spec/def ::set-ids (spec/and (spec/coll-of ::set-id)
                              set?))
(spec/def ::supersets ::set-ids)
(spec/def ::set-entry (spec/keys :req-un [::supersets]))
(spec/def ::set-map (spec/map-of ::set-id ::set-entry))
(spec/def ::element-entry (spec/keys ::req-un [::set-ids]))
(spec/def ::element-map (spec/map-of ::element-id ::element-entry))

(spec/def ::registry (spec/keys :req-un [::set-map ::element-map]))

(def empty-element-entry {:set-ids #{}})

(def empty-set-entry {:supersets #{}})

(def empty-set-registry {:set-map {}
                         :element-map {}})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-supersets [set-registry set-id]
  (-> set-registry
      :set-map
      :supersets))

(defn initialize-element [set-registry element]
  (update-in set-registry [:element-map element] #(or % empty-element-entry)))

(defn initialize-set [set-registry set-id]
  (update-in set-registry [:set-map set-id] #(or % empty-set-entry)))

(defn register-membership [set-registry element set-id]
  (-> set-registry
      (update-in [:element-map element :set-ids] #(conj % set-id))))


(defn register-superset [set-registry set-id-a set-id-b]
  (update-in set-registry [:set-map set-id-a :supersets] #(conj % set-id-b)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  High level API
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-registry? [x]
  (spec/valid? ::registry x))


(defn belongs-to [set-registry element set-id]
  (-> set-registry
      (initialize-element element)
      (initialize-set set-id)
      (register-membership element set-id)))

(defn subset-of [set-registry set-id-a set-id-b]
  (-> set-registry
      (initialize-set set-id-a)
      (initialize-set set-id-b)
      (register-superset set-id-a set-id-b)))

(defn supersets-of [set-registry set-ids]
  (transduce
   (comp (map (fn [set-id]
                (supersets-of
                 set-registry
                 (get-in set-registry [:set-map set-id :supersets]))))
         cat)
   conj
   set-ids
   set-ids))

(defn set-memberships [set-registry element]
  (set (supersets-of
        set-registry
        (get-in set-registry [:element-map element :set-ids]))))

(defn belongs-to? [set-registry element set-id]
  (contains? (set-memberships set-registry element) set-id))

(defn all-sets [set-registry]
  (-> set-registry
      :set-map
      keys
      set))

(comment
  (do
    
    (def a (belongs-to empty-set-registry :x :numbers))
    (def b (belongs-to a :x :elements))
    (def c (subset-of b :rational :numbers))

    )

  )
