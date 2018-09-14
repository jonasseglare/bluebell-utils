(ns examples.ebmd.example1
  (:require [bluebell.utils.ebmd :refer :all]))

;; We want to construct a generic 'abs' function

;; First some arg-specs

(def-arg-spec number-arg {:pred number?
                          :pos [3 4 -34 -30.4]
                          :neg [:a "asdf"]})

(def-arg-spec seq-arg {:pred sequential?
                       :pos [[3 34 ] '(:a :b 3)]
                       :neg [#{:a} {:a 3}]})

;; Declare the abs function

(declare-dispatch abs)

;; And the implementation for numbers

(def-dispatch abs [number-arg x]
  (Math/abs x))

(abs 119.0)
;; => 119.0

(abs -119.0)
;; => 119.0

(abs -3)
;; => 3

;; For sequential things, we do an elementwise abs:

(def-dispatch abs [seq-arg x]
  (mapv abs x))

(abs [1 2 -4 -30])
;; => [1 2 4 30]

;; And it also works with nested vectors
(abs [1 2 [-4 [[[[-119]]]]]])
;; => [1 2 [4 [[[[119]]]]]]

;; Let's introduce complex numbers
;; We are going to represent it as a vector starting with :complex, followed by the real and imaginary parts.
(defn complex? [x]
  (and (sequential? x)
       (= :complex (first x))
       (= 3 (count x))))

(def-arg-spec complex-arg {:pred complex?
                           :pos [[:complex 3 4]
                                 [:complex -3 4]]
                           :neg [[:complex]
                                 {}
                                 ]})

(def-dispatch abs [complex-arg [_ real imag]]
  (Math/sqrt (+ (* real real)
                (* imag imag))))

(abs [:complex 3 4])
;; => 5.0

(abs [:complex -3 -4])
;; => 5.0


(abs [-3 -4 -5])
;; => [3 4 5]

(print-arg-spec-comparison (samples abs)
                           [complex-arg
                            seq-arg])



(abs [[:complex 12 5]
      3
      [[[[[-3 [:complex 2 -4]]]]]]
      -119])
;; => [13.0 3 [[[[[3 4.47213595499958]]]]] 119]

;; Let's further extend it for other types, such as physical quantities.
;; Such as lengths:

(defn length? [x]
  (and (sequential? x)
       (= :meters (second x))
       (= 2 (count x))))

(def-arg-spec length-arg {:pred length?
                          :pos [[3.0 :meters]
                                [4.0 :meters]]
                          :neg [[] [:a]]})

(def-dispatch abs [length-arg [amount unit]]
  [(abs amount) unit])

(abs [-3 :meters])
;; => [3 :meters]

(abs [[:complex 3 4] :meters])
;; => [5.0 :meters]

(abs [[-3 -4 -4 [:complex 12 5]] :meters])
;; => [[3 4 4 13.0] :meters]

(abs [3
      -4
      [[-30
        [:complex -30 -40]] :meters]
      -100])
;; => [3 4 [[30 50.0] :meters] 100]




(def some-sample-values [ :a 3 4 [:a :b] [1] [] [:complex 3 4] [:complex 5 12] [:complex 0 1] :a])

(filter vector? some-sample-values)

(filter complex? some-sample-values)



(def-arg-spec anything {:pred (constantly true)
                        :pos [1 2 3 :a {} nil]
                        :neg []})

(declare-dispatch add)

(def-dispatch add [number-arg a
                   number-arg b]
  (+ a b))

(add 3 4)
;; => 7

(def-dispatch add [seq-arg a
                   any-arg b]
  (mapv (partial add b) a))

(add [1 2 3] 4)
;; => [5 6 7]

(def-dispatch add [seq-arg a
                   seq-arg b]
  (mapv add a b))

(add [1 2 3] [10 20 30])
;; => [11 22 33]

(def-dispatch add [complex-arg [_ a b]
                   complex-arg [_ x y]]
  [:complex (add a x) (add b y)])

(add [:complex 3 4] [:complex 30 40])
;; => [:complex 33 44]

(def-dispatch add [complex-arg [_ a b]
                   any-arg x]
  [:complex (add a x) b])

(def-dispatch add [any-arg x
                   complex-arg [_ a b]]
  [:complex (add a x) b])

(def-dispatch add [seq-arg x
                   complex-arg y]
  (mapv (partial add y) x))

(add [:complex 3 4] 110)
;; => [:complex 113 4]

(add [1 2 3 4] [:complex 3 4])
;; => [[:complex 4 4] [:complex 5 4] [:complex 6 4] [:complex 7 4]]

