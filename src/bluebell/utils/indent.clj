(ns bluebell.utils.indent
  (:require [bluebell.utils.string :as string]))

(def default-indent-settings {:prefix "  "})

(defn parse-indent-args [args]
  (if (map? (first args))
    [(first args) (rest args)]
    [default-indent-settings args]))

(defn indent-sub [[settings args]]
  `[::indent ~settings ~@args])

(defn indent [& args]
  (indent-sub (parse-indent-args args)))

(defn indent? [x]
  (and (vector? x)
       (= ::indent (first x))))

(defn prefix [x]
  (-> x second :prefix))

(defn body [x]
  (-> x rest rest))

(defn render-sub [p arg]
  (cond
    (indent? arg) (render-sub (str p (prefix arg))
                              (body arg))
    (coll? arg) (string/join-lines (map #(render-sub p %) arg))
    :default (str p arg)))

(defn render [& args]
  (render-sub "" args))
