(ns bluebell.utils.traverse
  (:require [bluebell.utils.core :as utils]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.party.coll :as partycoll]))

;;;;;;;;;;;;;;;;;;;;;;; Traversal
(declare traverse-postorder-cached-sub)

(defn traverse-postorder-cached-coll [m expr parent cfg]
  (utils/map-with-state
   (fn [m x]
     (traverse-postorder-cached-sub m x cfg parent))
   m
   expr))

(defn descend? [cfg expr]
  (let [d? (:descend? cfg)]
    (or (not d?) (d? expr))))

(defn add-parent [parents parent]
  (update
   parents parent
   (fn [n] (inc (or n 0)))))

(defn register-cached [orig cfg [m new-value] parent]
  [(assoc m orig {:mapped new-value
                  :parents (add-parent {} parent)}) new-value])

(defn look-up-and-inc [m expr parent]
  (let [{dst :mapped
         parents :parents} (get m expr)]
    [(assoc m expr {:mapped dst
                    :parents (add-parent parents parent)}) dst]))

(spec/def ::visit fn?)
(spec/def ::traverse-config (spec/keys :req-un [::visit]
                                       :opt-un [::access-coll]))



(defn traverse-postorder-cached-sub
  [m expr cfg parent]
  (if (contains? m expr)
    (look-up-and-inc m expr parent)
    (register-cached
     expr
     cfg (let [c ((:access-coll cfg) expr)
               [m c] (traverse-postorder-cached-coll m c expr cfg)]
           [m ((:visit cfg) ((:access-coll cfg) expr c))])
     parent)))

(def default-traverse-cfg {:access-coll partycoll/normalized-coll-accessor
                           :only-unique false})

(defn traverse-postorder-cached
  ([m expr cfg]
   (traverse-postorder-cached-sub m expr (merge default-traverse-cfg cfg) ::parent))
  ([expr cfg]
   (second (traverse-postorder-cached {} expr cfg))))
(spec/fdef traverse-postorder-cached :args
           (spec/cat
            :m (spec/? map?)
            :expr (constantly true)
            :cfg ::traverse-config))

;;;;;;;;;;;;; Helper utility on the cached one
(declare register-child-at)

(defn register-child-at-parents [m child at n]
  (let [parents (vec (get-in m [at :parents]))]
    (reduce
     (fn [m [k v]]
       (register-child-at m child k (* n v)))
     m
     parents)))

(defn register-child-here [m child at n]
  (update-in m [at :children child] (fn [x] (+ n (or x 0)))))

(defn register-child-at [m child at n]
  (register-child-here
   (register-child-at-parents m child at n)
   child
   at n))

(defn register-children [m]
  (reduce
   (fn [m c]
     (register-child-at m c c 1))
   m
   (keys m)))


;;;;;;;;;;;;;;;;;;;;; Traverse with state

(defn traverse-postorder-with-state-sub [state expr visit access]
  (let [[state children] (utils/map-with-state
                          (fn [state x]
                            (traverse-postorder-with-state-sub
                             state x visit access))
                          state (access expr))
        expr (access expr children)]
    (visit state expr)))

(defn process-config [cfg]
  (merge default-traverse-cfg cfg))

(defn traverse-postorder-with-state
  ([expr cfg]
   (let [c (process-config cfg)]
     (traverse-postorder-with-state-sub
      (:state c) expr (:visit c) (:access-coll c))))
  ([state expr cfg]
   (traverse-postorder-with-state
    expr (assoc cfg :state state))))

(def default-subexpr-cfg (merge default-traverse-cfg {:visit identity}))

(defn insert-subexpr [result p k]
  (update result p (fn [x] (conj (or x #{}) k))))

(defn add-subexpression-to-parents-of [result lookup p k]
  (if (= p ::parent)
    result
    (let [parents (keys (get-in lookup [p :parents]))]
      (reduce
       (fn [r x]
         (add-subexpression-to-parents-of r lookup x k))
       (insert-subexpr result p k)
       parents))))

(defn add-subexpression [result lookup k]
  (add-subexpression-to-parents-of result lookup k k))

(defn compute-subexpressions-sub [analyzed]
  (reduce (fn [r k]
            (add-subexpression r analyzed k))
          {}
          (keys analyzed)))

(defn compute-subexpressions
  ([expr]
   (compute-subexpressions expr {}))
  ([expr cfg]
   (compute-subexpressions-sub
    (first
     (traverse-postorder-cached
      {} expr (merge default-subexpr-cfg cfg))))))
