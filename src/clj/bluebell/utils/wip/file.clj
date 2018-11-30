(ns bluebell.utils.wip.file)

(defn exists? [x]
  (.exists (java.io.File. x)))
