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

(defn anti [x]
  (if (bubble? x)
    (break x)
    (bubble x)))

(defn protect-apply
  "Call f on the args if there is no bubble, else return the first bubble in the list."
  [f & args]
  (or (first (filter bubble? args))
      (apply f args)))

(defn protect-fn
  "Create a function that protect-apply'es f on the arguments."
  [f]
  (partial protect-apply f))

(defmacro protect-if
  "Use in place of an if-form, if the condition can be a bubble."
  ([c a b]
   `(let [x# ~c]
      (if (bubble? x#)
        x#
        (if x# ~a ~b))))
  ([c a]
   `(protect-if ~c ~a nil)))

(defn wrap-f? [f?]
  (fn [x] (if (f? x) [x])))

(defmacro filter-first [f & args]
  (let [fsym (gensym)]
    `(let [~fsym (wrap-f? ~f)]
       (or ~@(map (fn [x] `(~fsym ~x)) args)))))

(defmacro alts
  "Return the first value that is not a bubble. Don't evaluate the remaining ones."
  [& args]
  `(first (filter-first (complement bubble?) ~@args)))

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
