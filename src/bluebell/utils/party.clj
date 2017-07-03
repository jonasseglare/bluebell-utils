(ns bluebell.utils.party
  (:refer-clojure :exclude [update]))

(defn key-accessor [k]
  (fn
    ([obj]
     (get obj k))
    ([obj x]
     (assoc obj k x))))

(defn index-accessor [i]
  (fn
    ([obj] (nth obj i))
    ([obj x] (assoc obj i x))))

(defn chain2 [a b]
  (fn
    ([obj] (b (a obj)))
    ([obj x] (a obj (b (a obj) x)))))

(defn chain [& args]
  (reduce chain2 args))

(defn update [obj accessor f]
  (accessor obj (f (accessor obj))))
