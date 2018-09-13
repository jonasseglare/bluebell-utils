(ns bluebell.utils.ebo-test
  (:require [bluebell.utils.ebo :refer :all :as ebo]
            [clojure.test :refer :all]
            [bluebell.utils.specutils :as specutils]
            [clojure.spec.alpha :as spec]
            [clojure.set :as cljset]))

(spec/def ::mjao (partial = [:mjao 119]))

(deftest spec-test
  (is (spec/valid?
       ::ebo/arg-spec
       (normalize-arg-spec {:pred number?
                            :key :number
                            :pos [] :neg[]})))

  (let [k (#'ebo/decorate-key-and-pred-from-spec {:spec ::mjao})]
    (is (contains? k :key))
    (is (contains? k :pred))
    (is ((:pred k) [:mjao 119]))
    (is (not ((:pred k) [:mjao 118]))))
  (let [k (normalize-arg-spec {:spec ::mjao
                               :pos [[:mjao 119]]
                               :neg [9 :a {} {:a 3}]})]
    (is (:valid? k)))
  (let [k (normalize-arg-spec {:spec ::mjao
                               :pos [[:mjao 119] 4]
                               :neg [9 :a {} {:a 3}]})]
    (is (not (:valid? k))))
  (let [k (normalize-arg-spec {:spec ::mjao
                               :pos [[:mjao 119]]
                               :neg [9 :a {} {:a 3} [:mjao 119]]})]
    (is (not (:valid? k)))
    (is (thrown? Exception (check-valid k)))
    (is (= k (normalize-arg-spec k)))))

(def-arg-spec mummi {:pred number?
                     :desc "Any number"
                     :pos [9 3 1 -3 3/4]
                     :neg [:a :b]})

(deftest mummi-test
  (is (:valid? mummi))
  (is (= (-> mummi :key)
         [::ebo/def-arg-spec ::mummi]))
  (is (= [1 2 3 4] (filter-positive mummi [1 2 3 :a :b 4]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Overloading
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-arg-spec num-arg {:pred number?
                       :pos [3 4 3.4]
                       :neg [:a]})

(def-arg-spec vec-arg {:pred vector?
                       :pos [ [] ]
                       :neg [ :a ]})

(def-arg-spec a-vec-arg {:pred #(and (vector? %)
                                     (= :a (first %)))
                         :pos [ [:a 3 4] [:a] ]
                         :neg [ [] [:b] ]})

(defn negate-vec [v] (mapv - v))
(defn negate-a-vec  [[a & rf]]
  (into [a] (mapv - rf)))

(declare-overload my-negate)

(def-overload my-negate [vec-arg x]
  (negate-vec x))

(def-overload my-negate [a-vec-arg x]
  (negate-a-vec x))

(def-overload my-negate [num-arg x]
  (- x))

(deftest overload-state-test
  (let [s (#'ebo/init-overload-state 'kattskit)
        s (#'ebo/add-arg-spec s mummi)]
    (is (cljset/subset? #{3/4 :a :b} (:samples s)))
    (is (:dirty? s))
    (is (:key mummi))
    (is (= (:arg-specs s)
           {(:key mummi) mummi})))
  (let [s (#'ebo/init-overload-state 'negate)
        s (#'ebo/add-overload s {:arg-specs [mummi]
                                 :fn (fn [x] (- x))})]
    (is (= 1 (count (:arg-specs s))))
    (is (= 1 (count (:overloads s))))
    (is (= [(:key mummi)] (-> s :overloads (get 1) keys first)))
    (is (fn? (-> s :overloads (get 1) vals first))))
  (let [s (#'ebo/init-overload-state 'negate)
        s (#'ebo/add-overload s {:arg-specs [vec-arg]
                                 :fn negate-vec})
        s (#'ebo/add-overload s {:arg-specs [a-vec-arg]
                                 :fn negate-a-vec})]
    (is (= 2 (-> s :overloads (get 1) count)))
    (is (= #{[] :a [:b] [:a] [:a 3 4]}
           (:samples s)))
    (let [s (#'ebo/rebuild-arg-spec-samples s)
          s (#'ebo/rebuild-arg-spec-comparisons (#'ebo/unmark-dirty s))
          cmps (:arg-spec-comparisons s)
          s2 (#'ebo/rebuild-all s)]
      (is (= (get-in s [:arg-specs (:key vec-arg) :samples])
             #{[] [:b] [:a] [:a 3 4]}))
      (is (contains? s2 :overload-dominates?))
      (is (map? (:overload-dominates? s2)))
      (is (= 4 (count cmps)))
      (is (every?  #{:subset :superset :equal :disjoint}
                   (vals cmps)))
      (let [[arg-specs f] (#'ebo/resolve-overload
                           s2 [[1 2 3]])]
        (is (= arg-specs [(:key vec-arg)]))
        (is (= [-3 -4] (f [3 4]))))
      (let [[arg-specs f] (#'ebo/resolve-overload
                           s2 [[:a 3]])]
        (is (= arg-specs [(:key a-vec-arg)]))
        (is (= [:a -119] (f [:a 119]))))
      (is (thrown? Exception (#'ebo/resolve-overload s2 [{}])))
      (is (= [-3 -4]
             (#'ebo/evaluate-overload s2 [[3 4]]))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Complex example
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(spec/def ::complex-arg (spec/cat :prefix #{:complex}
                                  :real number?
                                  :imag number?))



;;;------- The arg types -------
(def-arg-spec number-arg {:pred number?
                          :pos [1]
                          :neg []})

(def-arg-spec neg-number-arg {:pred (fn [x]
                                      (and (number? x)
                                           (< x 0)))
                              :pos [-3 -4 -1]
                              :neg [3 4 :a 0]})

(def-arg-spec vector-arg {:pred vector?
                          :pos [[]]})


(def-arg-spec complex-arg {:spec ::complex-arg
                           :pos [[:complex 3.4 7.0]]})



;;;------- The overloads -------
(declare-overload abs)

(def-overload abs [number-arg x]
  (Math/abs x))

(def-overload abs [neg-number-arg x]
  (- x))

(def-overload abs [vector-arg x]
  (mapv abs x))

(def-overload abs [complex-arg [_ re im]]
  (Math/sqrt (+ (* re re)
                (* im im))))

;;;------- Tests -------
(deftest my-abs-test
  (is (= 3 (abs -3)))
  (is (= 3 (abs 3)))
  (is (= [3 4] (abs [3 -4])))
  (is (= [3 [[[4]]]] (abs [3 [[[-4]]]])))
  (is (= [3 [[[5.0]]]] (abs [3 [[[[:complex 3 4]]]]]))))
