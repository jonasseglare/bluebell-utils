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
(spec/def ::element-counter integer?)
(spec/def ::set-entry (spec/keys :req-un [::supersets ::element-counter]))
(spec/def ::set-map (spec/map-of ::set-id ::set-entry))
(spec/def ::element-entry (spec/keys ::req-un [::set-ids]))
(spec/def ::element-map (spec/map-of ::element-id ::element-entry))

(spec/def ::registry (spec/keys :req-un [::set-map ::element-map]))

(def empty-element-entry {:set-ids #{}})

(def empty-set-entry {:supersets #{}
                      :element-counter 0})

(def empty-set-registry {:set-map {}
                         :element-map {}})

;; Low level modifiers
(defn set-supersets [set-registry set-id]
  (-> set-registry
      :set-map
      :supersets))

(defn initialize-element [set-registry element]
  (update-in set-registry [:element-map element] #(or % empty-element-entry)))

(defn initialize-set [set-registry set-id]
  (update-in set-registry [:set-map set-id] #(or % empty-set-entry)))

(defn inc-counters-from
  ([set-registry] set-registry)
  ([set-registry set-id]
   (reduce
    inc-counters-from
    (update-in set-registry [:set-map set-id :element-counter] inc)
    (get-in set-registry [:set-map set-id :supersets]))))

(defn register-membership [set-registry element set-id]
  (-> set-registry
      (update-in [:element-map element :set-ids] #(conj % set-id))
      (inc-counters-from set-id)))

;; High level modifiers
(defn set-registry? [x]
  (spec/valid? ::registry x))

(defn belongs-to [set-registry element set-id]
  (-> set-registry
      (initialize-element element)
      (initialize-set set-id)
      (register-membership element set-id)))

(defn subset-of [set-registry set-id-a set-id-b])

(defn belongs-to? [set-registry element set-id])

(defn supersets-of [set-registry set-ids]
  (transduce
   (comp (map (fn [set-id]
                (println "Visiting set-id" set-id)
                (supersets-of
                 set-registry
                 (get-in set-registry [:set-map set-id :supersets]))))
         cat)
   conj
   set-ids
   set-ids))

(defn set-memberships [set-registry element]
  (supersets-of
   set-registry
   (get-in set-registry [:element-map element :set-ids])))

(comment
  (def a (belongs-to empty-set-registry :x :numbers))


  )
