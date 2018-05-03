(ns bluebell.utils.specutils
  (:require [clojure.spec.alpha :as spec]))

(defn force-conform [dst-spec x]
  (let [y (spec/conform dst-spec x)]
    (if (= y ::spec/invalid)
      (throw (ex-info "Failed to conform to spec"
                      {:spec dst-spec
                       :x x
                       :explanation (spec/explain-str dst-spec x)}))
      y)))



(defn validate [speck x]
  (if (not (spec/valid? speck x))
    (throw (ex-info "Spec validation failed"
                    {:explanation (spec/explain-str speck x)})))
  x)

(defn pred [speck]
  (fn [x] (spec/valid? speck x)))
