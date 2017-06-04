(ns epicea.utils.file)

(defn exists? [x]
  (.exists (java.io.File. x)))
