(ns bluebell.utils.ebmd-test
  (:require [bluebell.utils.ebmd :refer :all :as ebmd]
            [bluebell.utils.ebmd.ops :as ops]
            [bluebell.utils.ebmd.type :as type]
            [clojure.test :refer :all]
            [bluebell.utils.wip.specutils :as specutils]
            [clojure.spec.alpha :as spec]
            [clojure.set :as cljset]
            [bluebell.utils.wip.render-repl :refer [render]]))

(spec/def ::mjao (partial = [:mjao 119]))

(deftest spec-test
  (is (spec/valid?
       ::ebmd/arg-spec
       (import-arg-spec {:pred number?
                            :key :number
                            :pos [] :neg[]})))

  (let [k (#'ebmd/decorate-key-and-pred-from-spec {:spec ::mjao})]
    (is (contains? k :key))
    (is (contains? k :pred))
    (is ((:pred k) [:mjao 119]))
    (is (not ((:pred k) [:mjao 118]))))
  (let [k (import-arg-spec {:spec ::mjao
                               :pos [[:mjao 119]]
                               :neg [9 :a {} {:a 3}]})]
    (is (:valid? k)))
  (let [k (import-arg-spec {:spec ::mjao
                               :pos [[:mjao 119] 4]
                               :neg [9 :a {} {:a 3}]})]
    (is (not (:valid? k))))
  (let [k (import-arg-spec {:spec ::mjao
                               :pos [[:mjao 119]]
                               :neg [9 :a {} {:a 3} [:mjao 119]]})]
    (is (not (:valid? k)))
    (is (thrown? Exception (check-valid-arg-spec k)))
    (is (= k (import-arg-spec k)))))

;; (reset-registry!)

(def-arg-spec mummi {:pred number?
                     :desc "Any number"
                     :pos [9 3 1 -3 3/4]
                     :neg [:a :b]})

(def-arg-spec ::kattskit mummi)

(def-arg-spec ::kattskit-2 ::kattskit)

(deftest mummi-test
  (is (not (arg-spec? ::kattskit)))
  (is (arg-spec? (resolve-arg-spec ::kattskit)))
  (is (= (resolve-arg-spec ::kattskit)
         (resolve-arg-spec ::kattskit-2))) 
 (is (:valid? (resolve-arg-spec mummi)))
  (is (= (-> mummi resolve-arg-spec :key)
         [::ebmd/def-arg-spec ::mummi]))
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

(declare-poly my-negate)

(def-poly my-negate [vec-arg x]
  (negate-vec x))

(def-poly my-negate [a-vec-arg x]
  (negate-a-vec x))

(def-poly my-negate [num-arg x]
  (- x))

(deftest overload-state-test
  (let [s (#'ebmd/init-overload-state 'kattskit #{})
        s (#'ebmd/add-arg-spec s mummi)]
    ;(is (cljset/subset? #{3/4 :a :b} (set (:samples s))))
    (is (:dirty? s))
    
    ;;(is (:key mummi))
    (is (key? mummi))

    (is (= (keys (:arg-specs s))
           [(arg-spec-key mummi)])))
  (let [s (#'ebmd/init-overload-state 'negate #{})
        s (#'ebmd/add-overload s {:arg-specs [mummi]
                                 :fn (fn [x] (- x))})]
    (is (= (inc 1) (count (:arg-specs s))))
    (is (= 1 (count (:overloads s))))
    (is (= [(arg-spec-key mummi)]
           (-> s :overloads
               (get 2)
               keys
               first
               butlast)))
    (is (fn? (-> s :overloads (get 2) vals first :fn))))
  (let [s (#'ebmd/init-overload-state 'negate #{})
        s (#'ebmd/add-overload s {:arg-specs [vec-arg]
                                 :fn negate-vec})
        s (#'ebmd/add-overload s {:arg-specs [a-vec-arg]
                                  :fn negate-a-vec})]
    (is (= 2 (-> s :overloads (get 2) count)))

    ;; No samples if not rebuilt 
    #_(is (cljset/subset? #{[] :a [:b] [:a] [:a 3 4]}
                        (set (:samples s))))
    
    (let [s (#'ebmd/rebuild-all s)
          cmps (:arg-spec-comparisons s)
          s2 s]
      (is (= (get-in s [:arg-specs
                        (arg-spec-key vec-arg)
                        :samples])
             #{[] [:b] [:a] [:a 3 4]}))
      (is (contains? s2 :overload-dominates?))
      (is (map? (:overload-dominates? s2)))
      (is (= 9 (count cmps)))
      (is (every?  #{:subset :superset :equal :disjoint}
                   (vals cmps)))
      #_(let [[arg-specs ov] (#'ebmd/resolve-overload
                            s2 [[1 2 3 [1 2 3]]])
            f (:fn ov)]
        (is (= arg-specs [(:key vec-arg)]))
        (is (= [-3 -4] (f [3 4]))))
      #_(let [[arg-specs ov] (#'ebmd/resolve-overload
                            s2 [[:a 3]])
            f (:fn ov)]
        (is (= arg-specs [(:key a-vec-arg)]))
        (is (= [:a -119] (f [:a 119]))))
      (is (thrown? Exception (#'ebmd/resolve-overload s2 [{}])))
      (is (= [-3 -4]
             (#'ebmd/evaluate-overload s2 [[3 4]]))))))


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
(declare-poly abs)

(def-poly abs [any-arg0 x]
  [:undefined-abs x])

(def-poly abs [number-arg0 x]
  (Math/abs x))

(def-poly abs [vector-arg0 x]
  (mapv abs x))

(def-poly abs [complex-arg0 [_ re im]]
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

(declare-poly add)

(def-poly add [number-arg0 a
                   number-arg0 b]
  (+ a b))

(def-poly add [complex-arg0 [_ a-re a-im]
                   complex-arg0 [_ b-re b-im]]
  [:complex (add a-re b-re) (add a-im b-im)])

(def-poly add [complex-arg0 [_ re im]
                   imag-arg0 [_ x]]
  [:complex re (add im x)])

(def-poly add [imag-arg0 [_ x]
                   imag-arg0 [_ y]]
  [:imag (add x y)])

(def-poly add [complex-arg0 [_ re im]
                   any-arg0 b]
  [:complex (add re b) im])

(def-poly add [any-arg0 b
                   complex-arg0 [_ re im]]
  [:complex (add re b) im])

(def-poly add [vector-arg0 v
                   any-arg0 x]
  (mapv (partial add x) v))

(def-poly add [vector-arg0 a
                   vector-arg0 b]
  (mapv add a b))

(def-poly add [any-arg0 a
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

(comment
  
  (print-arg-spec-comparison 
   (samples add)
   [number-arg0
    complex-arg0
    vector-arg0])
  
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Test predefined arg specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare-poly plus)

(def-poly plus []
  0)

(def-poly plus [type/any x]
  x)

(def-poly plus [type/any a
                    type/any b]
  (+ a b))

(def-poly plus [type/string a
                    type/string b]
  (str a b))

(def-poly plus [type/coll a
                    type/coll b]
  (concat a b))

(def-poly plus [type/coll a
                    type/any b]
  (conj a b))

(def-poly plus [type/map a
                    type/map b]
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
(declare-poly sqr)

(def-poly sqr [type/any x]
  (* x x))

(def-poly sqr [(ops/not type/number) x]
  [:cannot-square x])

(deftest not-op-test
  (is (= 9 (sqr 3)))
  (is (= [:cannot-square "a"] (sqr "a"))))

(def-arg-spec zero-arg {:pred zero?
                        :pos [0]
                        :neg [1 2 3]})

(def denom-arg (ops/and type/number
                        (ops/not zero-arg)))

(def number-or-keyword-arg (ops/or type/number
                                   type/keyword))

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
;;;  Joint arguments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare-poly make-span)

(def-poly make-span [type/number a
                         type/number b]
  [a b])


(def-arg-spec reversed {:pred (fn [x]
                                (and (vector? x)
                                     (let [[a b] x]
                                       (and (number? a)
                                            (number? b)
                                            (< b a)))))
                        :pos [[3 2] [9 7]]
                        :neg [[0 1] [0 0]]})

(def-poly make-span [type/number a
                         type/number b

                         ;; Special case when the arguments are not ordered.
                         :joint reversed]
  (make-span b a))


(deftest joint-arg-test
  (is (= (make-span 3 4)
         [3 4]))
  (is (= (make-span 9 3)
         [3 9])))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Ambiguity
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare-poly amb)

(def-poly amb [type/any a
                   type/number b]
  [:b b])

(def-poly amb [type/number a
                   type/any b]
  [:a b])

(deftest check-it-is-ambiguous
  (is (thrown? Exception (amb 3 4))))

