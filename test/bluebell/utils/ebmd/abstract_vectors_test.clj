(ns bluebell.utils.ebmd.abstract-vectors-test
  (:require [bluebell.utils.ebmd :as ebmd]
            [clojure.test :refer :all]
            [bluebell.utils.ebmd.type :as etype]

            [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]))

(spec/def ::cartesian-2d (spec/and sequential?
                                   (spec/cat :x number?
                                             :y number?)))

(spec/def ::rho number?)
(spec/def ::phi number?)

(spec/def ::polar (spec/keys :req-un [::rho ::phi]))


;;;------- Arg specs for different types -------
(ebmd/def-arg-spec cartesian-vector {:pred sequential?

                                     ;; Positive examples (at least 1)
                                     :pos [[1 2] [] [[0 4 3]]]

                                     ;; Negative examples
                                     :neg [:a :b {} #{3 4}]})

(ebmd/def-arg-spec cartesian-2d-vector {:spec ::cartesian-2d

                                        :pos [[1 2] [3 4]]
                                        :neg [[1 2 3]]})

(defn polar-vector? [x]
  (and (map? x)
       (contains? x :rho)
       (contains? x :phi)))

(ebmd/def-arg-spec polar-vector {:pred polar-vector?
                                 :pos [{:rho 3.4 :phi 9}]
                                 :neg [{:a 3 :b 4} :a]})


;;;------- Angle -------
(ebmd/declare-poly angle)

(ebmd/def-poly angle [polar-vector x]
  (:phi x))

(ebmd/def-poly angle [cartesian-2d-vector [x y]]
  (Math/atan2 y x))

(ebmd/def-poly angle [cartesian-vector x]
  (throw (ex-info "What is the angle of a vector whose length is not 2?"
                  {:input x})))

(deftest angle-test
  (is (= 3.0 (angle {:rho 4.0 :phi 3.0})))
  (is (= (* 0.5 Math/PI) (angle [0 3]))))


