(ns bluebell.specs.indent
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::step number?)
(spec/def ::prefix string?)
(spec/def ::settings (spec/keys :req-un [::step ::prefix]))

