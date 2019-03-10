(ns bluebell.utils.ebmd-test
  (:import [bluebell.utils.ebmd ArgSpec])
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [clojure.set :as cljset]
            [bluebell.utils.ebmd.type :as type]
            [bluebell.utils.ebmd.ops :as ops]
            [clojure.set :as cljset]
            [bluebell.utils.ebmd :refer :all]))

(def-arg-spec ::any {:pred any?
                     :pos [1 2 3 :a "a"]
                     :neg []})

(def-arg-spec ::number {:pred number?
                        :pos [1 3/4 3.4 -4]
                        :neg [:a {:a 3}]})

(def-arg-spec ::int {:pred int?
                     :pos [1 2 3]
                     :neg [13.3 :a []]})

(def-arg-spec ::string {:pred string?
                        :pos ["a" "asdf" ""]
                        :neg [3 13.3 :a []]})

(deftest basic-ebmd-test
  (is (instance? ArgSpec (resolve-arg-spec ::int)))
  (is (instance? ArgSpec
                 (resolve-arg-spec
                  (resolve-arg-spec ::int))))
  (is (matches-arg-spec? ::int 3))
  (is (not (matches-arg-spec? ::int :a)))
  (is (cljset/subset? #{::int} (arg-spec-keys))))

(declare-poly add-)

(def-poly add- [::number a
               ::number b]
  [:sum (+ a b)])

(def-poly add- [::int a
               ::int b]
  (+ a b))

(def-poly add- [::string a
               ::string b]
  (str a b))

(def-poly add- [::any x]
  x)

(register-promotion ::string str ::int)

(deftest add-test
  (is (= 7 (add- 3 4)))
  (is (= [:sum 3.4] (add- 3 0.4)))
  (is (= "kattsk1t" (add- "katt" "sk1t")))
  (is (= "kattsk1" (add- "kattsk" 1)))
  (is (= :katt (add- :katt))))

;; (poly-summary add)
;; (print-arg-spec-comparison [::int ::number ::string])
;; (print-arg-spec-comparison (poly-arg-specs add-))
  






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
                           :pos [[]]
                           :neg []})


(def-arg-spec complex-arg0 {:spec ::complex-arg
                            :pos [[:complex 3.4 7.0]]
                            :neg []})



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


(deftest my-abs-test
  (is (= 3 (abs -3)))
  (is (= 3 (abs 3)))
  (is (= [3 4] (abs [3 -4])))
  (is (= [3 [[[4]]]] (abs [3 [[[-4]]]])))
  (is (= [3 [[[5.0]]]] (abs [3 [[[[:complex 3 4]]]]])))
  (is (= [3 4 [:undefined-abs :a]]
         (abs [-3 4 :a]))))

(deftest meta-abs-test
    (is (set? (poly-samples abs)))
    (is (= #{1} (poly-arities abs))))

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

(def-arg-spec zero-arg {:pred (fn [x]
                                (and (number? x)
                                     (zero? x))) 
                        :pos [0]
                        :neg [1 2 3]})

(def darg (ops/not zero-arg))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Promotions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-promotion ::basic-string-prom
                    str
                    ::int-prom)

(register-promotion ::string-prom
                    str
                    ::float-prom)

(register-promotion ::int-prom
                    (fn [x] (if x 1 0))
                    ::boolean-prom)

(register-promotion ::float-prom
                    double
                    ::int-prom)


(def-arg-spec ::basic-string-prom {:pred string?
                                   :pos ["aasdf" ""]
                                   :neg [9 3]})

(def-arg-spec ::string-prom ::basic-string-prom)

(def-arg-spec ::float-prom {:pred double?
                            :pos [3.0 4.3]
                            :neg [3 4 false "asdf"]})

(def-arg-spec ::int-prom {:pred int?
                          :pos [9 3]
                          :neg ["asdf"]})

(def-arg-spec ::boolean-prom {:pred boolean?
                              :pos [true false]
                              :neg [9 0]})

(declare-poly prom-add)

(def-poly prom-add [::string-prom a
                    ::string-prom b]
  (str a b))

(def-poly prom-add [::boolean-prom a
                    ::boolean-prom b]
  (or a b))


(deftest auto-promotions
  (is (= "01" (prom-add false 1)))
  (is (= "3.43.3" (prom-add 3.4 3.3)))
  (is (= "3.40" (prom-add 3.4 false)))
  (is (= false (prom-add false false)))
  (is (= true (prom-add false true)))
  (is (= "katt0" (prom-add "katt" false)))
  (is (= "kattskit" (prom-add "katt" "skit"))))



;;;------- Ambiguity with promotion -------

(def-arg-spec prom-seq {:pred sequential?
                        :pos [[1 2 3]]
                        :neg [#{:a}]})

(def-arg-spec prom-set {:pred set?
                        :pos [#{3 4}]
                        :neg [{:a 3}]})

(def-arg-spec prom-map {:pred map?
                        :pos [{:a 4}]
                        :neg [#{9 3}]})

(def-arg-spec prom-num {:pred number?
                        :pos [9.9]
                        :neg [false]})

(register-promotion prom-seq
                    vec
                    prom-set)

(register-promotion prom-seq
                    vec
                    prom-map)

(register-promotion prom-map
                    (fn [x] {x x})
                    prom-num)

(register-promotion prom-set
                    (fn [x] #{x})
                    prom-num)

(def number-types [::pref-byte ::pref-short ::pref-int ::pref-long ::pref-float ::pref-double])

(def all-promotions (reduce into []
                            (for [[from & tos]
                                  (take-while
                                   (complement empty?)
                                   (iterate rest number-types))]
                              (for [to tos]
                                [from to]))))

(doseq [[from to] all-promotions]
  (register-promotion to
                      (partial vector to)
                      from))

(defn num-arg-spec [k cl sample]
  {:pred (fn [x] (or (= cl (class x))
                     (and (vector? x)
                          (= k (first x)))))
   :pos [sample]
   :neg []})

(def-arg-spec ::pref-double (num-arg-spec ::pref-double Double 3.0))
(def-arg-spec ::pref-float (num-arg-spec ::pref-float Float
                                         (float 3.0)))
(def-arg-spec ::pref-long (num-arg-spec ::pref-long Long (long 3)))
(def-arg-spec ::pref-int (num-arg-spec ::pref-int Integer (int 3)))
(def-arg-spec ::pref-byte (num-arg-spec ::pref-byte Byte (byte 3)))
(def-arg-spec ::pref-short (num-arg-spec ::pref-short
                                         Short (short 3)))

(declare-poly add-0)

(def-poly add-0 [::pref-double a
                 ::pref-double b]
  [:add a b])

;; (add-0 3 4)

;; (promotion-path ::pref-double 3)

(deftest promotion-shortest-path-test
  (is [:add [::pref-double 3] [::pref-double 4]]
      (add-0 3 4)))



(def-arg-spec ::pref-sp-number {:pred number?
                                :pos [3 4]
                                :neg [:a]
                                :reg-spec? true})



;;;------- Extensions -------

(def-arg-spec-union ::real-numbers)

(def-arg-spec ::clj-number {:pred number?
                            :pos [3 4 3.4 3/4]
                            :neg [:a :b {:a 3}]})

(extend-arg-spec ::real-numbers ::clj-number)
(extend-arg-spec ::real-numbers ::dot-count)

(declare-poly tag-real-numbers)

(def-poly tag-real-numbers [any-arg x]
  [:not-a-number x])

(def-poly tag-real-numbers [::real-numbers x]
  [:real-number x])

(defn dot-count? [x]
  (and (string? x)
       (every? (partial = \.) x)))

(def-arg-spec ::dot-count {:pred dot-count?
                           :pos #{"" "..." "......"}
                           :neg #{"adsf" #{3 4} :a}})

(deftest union-test
  (is (= [:real-number 9.0] (tag-real-numbers 9.0)))
  (is (= [:not-a-number :a] (tag-real-numbers :a)))
  (is (= [:real-number "..."] (tag-real-numbers "..."))))


(declare-poly do-something)

(def-poly do-something [::any x]
  x)

(def-poly do-something [::type/fn f]
  (f))

(deftest fn-type-test
  (is (= 9 (do-something #(* 3 3))))
  (is (= 9 (do-something 9))))
