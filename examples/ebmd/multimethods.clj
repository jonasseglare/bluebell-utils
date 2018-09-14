(ns examples.ebmd.multimethods)

(defmulti abs (fn [x] (:type x)))

(defmethod abs :real [x]
  (Math/abs (:value x)))


(defn sqr [x] (* x x))

(defmethod abs :complex [x]
  (Math/abs (+ (sqr (:real x))
               (sqr (:imag x)))))

(abs {:type :number :value -119})
;; => 119

(abs {:type :complex :real 3 :imag -4})
;; => 25

;(abs 3)

;; => Exception: No method in multimethod 'abs' for dispatch value: null




(defn more-generic-dispatch [x]
  (cond
    (number? x) :number
    (map? x) (:type x)))
