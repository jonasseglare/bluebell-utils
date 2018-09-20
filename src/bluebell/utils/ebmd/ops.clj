(ns bluebell.utils.ebmd.ops
  (:require [bluebell.utils.ebmd :as ebo]
            [bluebell.utils.wip.core :as utils])
  (:refer-clojure :exclude [and or not]))


(defn- transform-arg-spec [tag pred-maker arg-specs]
  (utils/check-io
   [:pre [(fn? pred-maker)]
    :post out [::ebo/arg-spec out]]

   (let [arg-specs (map ebo/normalize-and-check-arg-spec
                        arg-specs)]
     (ebo/normalize-and-check-arg-spec
      (ebo/provide-samples
       {:key (into [tag] (map :key arg-specs))
        :pred (pred-maker (map :pred arg-specs))}
       (reduce into #{} (map ebo/arg-spec-samples arg-specs)))))))

(defn- make-not-pred [input-preds]
  (complement (first input-preds)))

(defn- make-and-pred [input-preds]
  (fn [x]
    (every? (fn [p] (p x)) input-preds)))

(defn- make-or-pred [input-preds]
  (fn [x]
    (some (fn [p] (p x)) input-preds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn not [arg-spec]
  (transform-arg-spec
   :not
   make-not-pred
   [arg-spec]))

(defn and [& arg-specs]
  (transform-arg-spec
   :and
   make-and-pred
   arg-specs))

(defn or [& arg-specs]
  (transform-arg-spec
   :or
   make-or-pred
   arg-specs))

(defn implies [a b]
  (or (not a) b))
