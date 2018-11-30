(ns bluebell.utils.ebmd.java
  (:import [bluebell.utils.ebmd Registry Settings]))

(def registry (Registry. (.release (Settings.))))
