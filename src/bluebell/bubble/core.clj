(ns bluebell.bubble.core
  (:require [bluebell.utils.core :as core]))

(defn bubble
  "Make a bubble around x."
  [x]
  [::bubble x])

(defn bubble?
  "Test if x is a bubble."
  [x]
  (and (vector? x)
       (= ::bubble (first x))))

(defn break
  "Break a bubble."
  [x]
  (assert (bubble? x))
  (second x))

(defn protect-apply
  "Call f on the args if there is no bubble, else return the first bubble in the list."
  [f & args]
  (or (first (filter bubble? args))
      (apply f args)))

(defn protect-fn
  "Create a function that protect-apply'es f on the arguments."
  [f]
  (partial protect-apply f))

(defn to-nil-or-vec [x]
  (if (not (bubble? x))
    [x]))

(defn anti
  "Make a bubble from a non-bubble, or break a bubble."
  [x]
  (if (bubble? x)
    (break x)
    (bubble x)))

(defmacro alts
  "Return the first value that is not a bubble. Don't evaluate the remaining ones."
  [& args]
  `(first (or ~@(map (fn [x] `(to-nil-or-vec ~x)) args))))

(defn contains-bubble? [x]
  "Test if there is a bubble nested inside a data structure."
  (first
   (core/traverse-postorder-with-state
    x
    {:visit (fn [state x]
              (if (bubble? x)
                [true x]
                [state x]))
     :state false})))

(defn bubble-up
  "If there is a bubble inside a datastructure, wrap a bubble around that datastructure."
  [x]
  (if (and (not (bubble? x))
           (contains-bubble? x))
    (bubble x)
    x))
