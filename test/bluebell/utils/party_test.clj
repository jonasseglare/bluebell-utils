(ns bluebell.utils.party-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer :all]
            [bluebell.utils.party :refer :all]))

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

