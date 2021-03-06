(ns bluebell.utils.wip.data-factors
  "Find factors in data structures"
  (:require [bluebell.utils.wip.traverse :as traverse]
            [bluebell.utils.wip.party.coll :as partycoll]
            [clojure.walk :as walk]
            [clojure.pprint :as pp]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn identify-factors [analysis]
  (transduce
   (comp (filter (fn [[k v]] (< 1 (:count v))))
         (map first))
   conj
   []
   analysis))

(defn make-replacement-map [factors]
  (zipmap factors
          (map (fn [i]
                 (keyword (str *ns*)
                          (format "factor-%d" i)))
               (range (count factors)))))

(defn replace-factors [dst replacements0]
  (let [replacements (dissoc replacements0 dst)]
    (walk/prewalk
     (fn [x] (if (contains? replacements x)
               (get replacements x)
               x))
     dst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn factorize
  ([x] (factorize x {}))
  ([x traverse-settings]
   (let [analysis (first (traverse/traverse-postorder-cached
                          {}
                          x
                          (merge traverse-settings
                                 {:visit identity})))
         factors (identify-factors analysis)
         replacements (merge {x ::top}
                             (make-replacement-map factors))
         replacer #(replace-factors % replacements)
         replaced (map (fn [[k v]] [v (replacer k)]) replacements)
         ]
     (into {} replaced))))

(def disp (comp pp/pprint factorize))
