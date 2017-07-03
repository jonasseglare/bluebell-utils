(ns bluebell.utils.party
  (:refer-clojure :exclude [update]))

(defn key-accessor
  "Create an accessor for map keys"
  [k]
  (fn
    ([obj]
     (get obj k))
    ([obj x]
     (assoc obj k x))))

(defn index-accessor
  "Create an accessor for indices"
  [i]
  (fn
    ([obj] (nth obj i))
    ([obj x] (assoc obj i x))))

(defn chain2 [a b]
  (fn
    ([obj] (b (a obj)))
    ([obj x] (a obj (b (a obj) x)))))

(defn chain [& args]
  "Connect accessors in a chain"
  (reduce chain2 args))

(defn update [obj accessor f]
  "Use an accessor to update an object"
  (accessor obj (f (accessor obj))))

;;;;;;;;;;;;;;;;;;;;;; Decorators
