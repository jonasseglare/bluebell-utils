(ns bluebell.utils.wip.edn
  (:require [clojure.edn :as edn]))

(defn load-file [file]
  (-> file
      slurp
      edn/read-string))

(defn save-file [file data]
  (binding [*print-length* nil
            *print-level* nil]
    (spit file data)))
