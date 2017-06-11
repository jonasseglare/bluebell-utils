(ns nettle.fibers.types
  (:require [nettle.fibers.core :refer :all])
  (:require [clojure.spec :as spec])
  (:require [nettle.utils.core :as utils]))

(def primitive-traits {:primitive? true})
(def primitive-number-traits (merge 
                              primitive-traits
                              {:scalar? true}))

(def primitive-list [[[:bool] primitive-traits]
                     [[:float :float32] primitive-number-traits]
                     [[:float64 :double] primitive-number-traits]
                     [[:int32 :int] primitive-number-traits]
                     [[:int64 :long] primitive-number-traits]])

(defn scalar? [x]
  (:scalar? x))

(defn primitive? [x]
  (:primitive? x))

(defn add-primitive-entry [dst [names properties]]
  (reduce
   (fn [dst name]
     (assoc dst name properties))
   dst
   names))

(def primitives (reduce add-primitive-entry {} primitive-list))

(defn primitive-type? [x]
  (contains? primitives x))

(defn with-traits [t x]
  (if-let [y (get primitives t)]
    (merge x y)
    (utils/common-error "Not a primitive: " t)))

(defn primitive
  ([t] (assert (primitive-type? t))
   (with-traits t (primitive-expr t)))
  ([t x] 
   (assert (primitive-type? t))
   (with-traits t (primitive-expr t x))))

(spec/def ::scalar-map (spec/map-of (constantly true) scalar?))

(defn ad-sub [x derivative-map]
  (assert (scalar? x))
  (assert (spec/valid? ::scalar-map derivative-map))
  {:type :ad :x x :derivatives derivative-map})

(defn ad 
  ([x] (ad x {}))
  ([x derivative-map] (ad-sub x derivative-map)))

(defn ad? [x]
  (= (:type x) :ad))
