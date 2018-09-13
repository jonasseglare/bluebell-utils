(ns bluebell.utils.ebo-test
  (:require [bluebell.utils.ebo :refer :all :as ebo]
            [bluebell.utils.ebo.ops :as ops]
            [clojure.test :refer :all]
            [bluebell.utils.specutils :as specutils]
            [clojure.spec.alpha :as spec]
            [clojure.set :as cljset]
            [bluebell.utils.render-repl :refer [render]]))

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
    (is (thrown? Exception (check-valid-arg-spec k)))
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

(def-arg-spec vec-arg {:pred sequential?
                       :pos [ [] ]
                       :neg [ :a ]})

(def-arg-spec a-vec-arg {:pred #(and (sequential? %)
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
    (is (cljset/subset? #{3/4 :a :b} (set (:samples s))))
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
           (set (:samples s))))
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
                                  :real any?
                                  :imag any?))



;;;------- The arg types -------
(def-arg-spec any-arg0 {:pred (constantly true)
                        :pos [nil false true {} []]
                        :neg []})

(def-arg-spec number-arg0 {:pred number?
                           :pos [1]
                           :neg []})

(def-arg-spec neg-number-arg0 {:pred (fn [x]
                                       (and (number? x)
                                            (< x 0)))
                               :pos [-3 -4 -1]
                               :neg [3 4 :a 0]})

(def-arg-spec vector-arg0 {:pred sequential?
                           :pos [[]]})


(def-arg-spec complex-arg0 {:spec ::complex-arg
                            :pos [[:complex 3.4 7.0]]})


;;;------- The overloads -------
(declare-overload abs)

(def-overload abs [any-arg0 x]
  [:undefined-abs x])

(def-overload abs [number-arg0 x]
  (Math/abs x))

(def-overload abs [vector-arg0 x]
  (mapv abs x))

(def-overload abs [complex-arg0 [_ re im]]
  (Math/sqrt (+ (* re re)
                (* im im))))

;;;------- Tests -------
(deftest my-abs-test
  (is (= 3 (abs -3)))
  (is (= 3 (abs 3)))
  (is (= [3 4] (abs [3 -4])))
  (is (= [3 [[[4]]]] (abs [3 [[[-4]]]])))
  (is (= [3 [[[5.0]]]] (abs [3 [[[[:complex 3 4]]]]])))
  (is (= [3 4 [:undefined-abs :a]]
         (abs [-3 4 :a]))))

(deftest meta-abs-test
  (is (set? (samples abs)))
  (is (= #{1} (arities abs))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Generic add
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;------- More specs -------

(def-arg-spec imag-arg0 {:pred (fn [x] (and (sequential? x)
                                            (= :imag (first x))))
                         :pos [
                               
                               [:imag 3]
                               
                               [:imag :a]

                               ]
                         
                         :neg [[] 9 34]})

(declare-overload add)

(def-overload add [number-arg0 a
                   number-arg0 b]
  (+ a b))

(def-overload add [complex-arg0 [_ a-re a-im]
                   complex-arg0 [_ b-re b-im]]
  [:complex (add a-re b-re) (add a-im b-im)])

(def-overload add [complex-arg0 [_ re im]
                   imag-arg0 [_ x]]
  [:complex re (add im x)])

(def-overload add [imag-arg0 [_ x]
                   imag-arg0 [_ y]]
  [:imag (add x y)])

(def-overload add [complex-arg0 [_ re im]
                   any-arg0 b]
  [:complex (add re b) im])

(def-overload add [any-arg0 b
                   complex-arg0 [_ re im]]
  [:complex (add re b) im])

(def-overload add [vector-arg0 v
                   any-arg0 x]
  (mapv (partial add x) v))

(def-overload add [vector-arg0 a
                   vector-arg0 b]
  (mapv add a b))

(def-overload add [any-arg0 a
                   vector-arg0 b]
  (add b a))

(deftest generic-add-test
  (is (= 7 (add 3 4)))
  (is (= [:complex 30 20]
         (add [:complex 10 5]
              [:complex 20 15])))
  (is (= (add [1 2 3] 10)
         [11 12 13]))
  (is (= (add [1 2 3] [100 0 1000])
         [101 2 1003]))
  (is (= (add 10 [1 2 3])
         [11 12 13]))
  (is (= [:complex 1001 3]
         (add [:complex 1 3]
              1000)))
  (is (= [:complex 1001 3]
         (add 1000 [:complex 1 3])))
  (is (= [:complex 3 20]
         (add [:complex 3 1]
              [:imag 19])))
  (is (= [:imag 20]
         (add [:imag 3]
              [:imag 17]))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Test predefined arg specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare-overload plus)

(def-overload plus []
  0)

(def-overload plus [any-arg x]
  x)

(def-overload plus [any-arg a
                    any-arg b]
  (+ a b))

(def-overload plus [string-arg a
                    string-arg b]
  (str a b))

(def-overload plus [coll-arg a
                    coll-arg b]
  (concat a b))

(def-overload plus [coll-arg a
                    any-arg b]
  (conj a b))

(def-overload plus [map-arg a
                    map-arg b]
  (merge a b))

(deftest predef-arg-test
  (is (= 0 (plus)))
  (is (= 7 (plus 3 4)))
  (is (= [:a :b :c] (plus [:a :b] :c)))
  (is (= [:a :b 4] (plus [:a :b] [4])))
  (is (= {:a 3 :b 4} (plus {:a 3} {:b 4})))
  (is (= "kycklinglever" (plus "kyckling" "lever"))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Ops test
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare-overload sqr)

(def-overload sqr [any-arg x]
  (* x x))

(def-overload sqr [(ops/not number-arg) x]
  [:cannot-square x])

(deftest not-op-test
  (is (= 9 (sqr 3)))
  (is (= [:cannot-square "a"] (sqr "a"))))

(def-arg-spec zero-arg {:pred zero?
                        :pos [0]
                        :neg [1 2 3]})

(def denom-arg (ops/and number-arg
                        (ops/not zero-arg)))

(def number-or-keyword-arg (ops/or
                            number-arg
                            keyword-arg))

(deftest and-or-test
  (is (matches-arg-spec? denom-arg 3))
  (is (not (matches-arg-spec? denom-arg 0)))
  (is (not (matches-arg-spec? denom-arg :a)))
  (is (matches-arg-spec? number-or-keyword-arg :a))
  (is (matches-arg-spec? number-or-keyword-arg 9))
  (is (not (matches-arg-spec? number-or-keyword-arg []))))

(def-arg-spec second-is-number {:pred (fn [x] (and (vector? x)
                                                   (number? (second x))))
                                :pos [[nil 1] [:a 3]]
                                :neg [[nil :a]]})

(def-arg-spec first-is-number {:pred (fn [x] (and (vector? x)
                                                   (number? (first x))))
                                :pos [[1] [3 :a]]
                               :neg [[nil :a]]})

(def imp (ops/implies first-is-number second-is-number))

(deftest implies-test
  (is (matches-arg-spec? imp [3 4]))
  (is (matches-arg-spec? imp [nil 4]))
  (is (matches-arg-spec? imp [nil :a]))
  (is (not (matches-arg-spec? imp [3 :a]))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Produce code for the repl example
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://emacs.stackexchange.com/questions/36112/is-it-possible-on-cider-to-eval-on-a-repl-and-print-as-a-comment-every-evaluatio

(comment
  
  (def-arg-spec numeric-argument {:pred number?
                                  :pos [1 2 3 -4 3/4]
                                  :neg [nil {} #{} []]})
  
  
  (def-arg-spec seq-argument {:pred sequential?
                              :pos [[] [:a :b] '(3 4)]
                              :neg [#{} {}]})
  
  (def-arg-spec complex-argument {:pred (fn [x]
                                          (and (sequential? x)
                                               (= :complex (first x))))
                                  :pos [[:complex 3 4]]
                                  :neg [[] :a :b]})


  ;; C-u C-x C-e
  


  )
