(ns bluebell.utils.party-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer :all]
            [clojure.spec :as spec]
            [bluebell.utils.party :refer :all :as party]))

(def mjao (key-accessor :mjao))

(deftest key-test
  (is (= 9 (mjao {:mjao 9})))
  (is (= {:mjao 9} (mjao {} 9))))

(def katt (index-accessor 2))

(deftest index-test
  (is (= 9 (katt [1 2 9 0 4]))))

(def mjao-katt (chain2 mjao katt))

(deftest mjao-katt-test
  (is (= 2 (mjao-katt {:mjao [0 1 2 3 4]})))
  (is (= {:mjao [0 1 9 3 4]} (mjao-katt {:mjao [0 1 2 3 4]} 9)))
  (is (= {:mjao [0 1 3]} (update {:mjao [0 1 2]} mjao-katt inc))))

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
