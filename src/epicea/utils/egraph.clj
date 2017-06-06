(ns epicea.utils.egraph
  (:require [clojure.spec :as spec]))

(spec/def ::unique-tag (partial = ::node))

(spec/def ::node (spec/keys :req-un [::unique-tag]))

(defn node? [x]
  (spec/valid? ::node x))
