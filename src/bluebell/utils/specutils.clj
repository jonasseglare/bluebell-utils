(ns bluebell.utils.specutils
  (:require [clojure.spec.alpha :as spec]))

(defn force-conform [dst-spec x]
  (let [y (spec/conform dst-spec x)]
    (if (= y ::spec/invalid)
      (throw (ex-info "Failed to conform to spec"
                      {:spec dst-spec
                       :x x}))
      y)))


