(ns examples.ebmd.example0
  (:require [bluebell.utils.wip.ebmd :refer :all]))

;; Let's start with some arguments

;; Any value
(def-arg-spec anything {:pred (constantly true)
                        :pos [3 4 5 {:a 9} [3 4 5] nil]
                        :neg []})

;; A number
(def-arg-spec number-arg {:pred number?
                          :pos [3 4 -3 0.3]
                          :neg [:a "asdfasdf"]})

;; Vectors and seqs
(def-arg-spec seq-arg {:pred sequential?
                       :pos [[3 4] '() '(234 4 )]
                       :neg [:a {} #{:a} 2]})

;; Let's declare a polymorphic add function
(declare-poly add)

;; Typically, when no arguments are provided, we may want to start adding to 0:
(def-poly add [] 0)

(add)
;; => 0

;; If we provide just a single argument, no matter what it is, we probably want to return it

(def-poly add [anything x]
  x)

(add 9)
;; => 9

(add :kattskit)
;; => :kattskit

(add [[[{:mu 119}]]])
;; => [[[{:mu 119}]]]

;; Lets define it so that we can add numbers, too:
(def-poly add [number-arg a
                   number-arg b]
  (+ a b))

(add 3 4)
;; => 7

(add 100 30)
;; => 130

;; If we get a pair of sequences, we probably want to add them element-wise
(def-poly add [seq-arg a
                   seq-arg b]
  (mapv add a b))


(add [1 2 3] [100 10 10])
;; => [101 12 13]

(add [] [])
;; => []

;; It even works with nested sequences
(add [[1] [2]] [[4] [20]])
;; => [[5] [22]]

;; Adding a number to a vector does not work yet:
;(add 3 [1 2 3])
;; => Exception: No overload found

;; So let's define that too...
(def-poly add [anything x
                   seq-arg y]
  (mapv (partial add x) y))

;; ... for both argument orders.
(def-poly add [seq-arg x
                   anything y]
  (mapv (partial add y) x))

(add 1000 [3 4 5])
;; => [1003 1004 1005]

(add [3 4 5] 1000)
;; => [1003 1004 1005]

;;;;; Let's add a new type, a length. Here is a map
;;;;; that defines different length units:

(def length-to-si {:m 1.0
                   :cm 0.01
                   :km 1000.0
                   :mm 0.001
                   :nm 1852.0})

;; We are going to represent lengths using a vector, e.g.
;; [3.0 :m] means "3.0 metres"

(defn length? [x]
  (and (vector? x)
       (= 2 (count x))
       (number? (first x))
       (contains? length-to-si (second x))))

(length? []);; => false

(length? 3)
;; => false

(length? [3.0 :m])
;; => true

(def-arg-spec length-arg {:pred length?
                          :pos [[3.0 :m] [0.1 :cm]]
                          :neg [[] #{:a :b}]})

;; This is a helper function to convert the unit of a length to SI:
(defn to-si [len]
  {:pre [(length? len)]}
  (let [[amount unit] len]
    [(* amount (unit length-to-si)) :m]))

(to-si [30.0 :cm])
;; => [0.3 :m]

;; Let's define general additon of lengths
(def-poly add [length-arg a
                   length-arg b]
  (let [[amount-a _] (to-si a)
        [amount-b _] (to-si b)]
    [(add amount-a amount-b) :m]))

(add [3.0 :m] [60.0 :cm])
;; => [3.6 :m]

(add [3.0 :mm] [4.0 :mm])
;; => [0.007 :m]

;; Maybe we want to, in the special case of the unit being the same,
;; avoid converting it to SI. We can to that with a "joint" arg spec
;; over all arguments

(def-arg-spec same-unit-arg {:pred (fn [x]
                                     (and (sequential? x)
                                          (let [[a b] x]
                                            (and (length? a)
                                                 (length? b)
                                                 (= (second a)
                                                    (second b))))))
                             :pos [
                                   [[3.0 :m] [4.0 :m]]
                                   ]
                             :neg [
                                   [[3.0 :m] [4.0 :cm]]
                                   ]})

(def-poly add [length-arg a
                   length-arg b

                   :joint same-unit-arg]
  (let [[x u] a
        [y u] b]
    [(add x y) u]))

(add [3.0 :cm] [4.0 :cm])
;; => [7.0 :cm]

;; What happens if we add a vector of lengths to a length?


