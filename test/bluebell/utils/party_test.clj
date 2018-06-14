(ns bluebell.utils.party-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.party :refer :all :as party]))

(def mjao (key-accessor :mjao {:req-on-get false}))

(deftest key-test
  (is (= 9 (mjao {:mjao 9})))
  (is (= {:mjao 9} (mjao {} 9)))
  (is (= {:mjao 119} (mjao nil 119))))

(def katt (default-value (index-accessor 2) 0))

(deftest index-test
  (is (nil? (katt nil)))
  (is (= 9 (katt [1 2 9 0 4])))
  (is (= [nil nil 45] (katt nil 45))))

(def mjao-katt (chain2 mjao katt))

(deftest mjao-katt-test
  (is (= 2 (mjao-katt {:mjao [0 1 2 3 4]})))
  (is (= {:mjao [0 1 9 3 4]} (mjao-katt {:mjao [0 1 2 3 4]} 9)))
  (is (= {:mjao [0 1 3]} (update {:mjao [0 1 2]} mjao-katt inc)))
  (is (= {:mjao [nil nil 1]}
         (update nil mjao-katt inc))))

(def mjao2 (-> mjao
               (validate-base map?)
               (validate-target int?)))

(deftest validated-test
  (is (= 9 (mjao2 {:mjao 9})))
  (is (= {:mjao 9} (mjao2 {} 9))))

(deftest arg-parse-test
  (is (spec/valid?
       ::party/args
       '(mjao {:valid? []} name phone {:valid? string?}))))

(defpseudorec gos
  nam {:valid? string?}
  age {:valid? int?})

(deftest gos-test
  (is (= {:nam "mummi"
          :age nil}
         (nam gos "mummi")))
  (is (= {:nam nil
          :age 31}
         (age gos 31))))

(def ak (default-value (key-accessor :a) 119))
(def bk (default-value (key-accessor :b) :bla))

(def abk-default (build-default-value ak bk))

(deftest default-value-test
  (is (= abk-default {:a 119 :b :bla})))
