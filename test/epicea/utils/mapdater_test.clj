(ns epicea.utils.mapdater-test
  (:require [clojure.test :refer :all]
            [epicea.utils.mapdater :refer :all :as md]
            [clojure.spec :as spec]))

(deftest spec-test2
  (is (= (spec/conform ::md/mapdater '(a [b c] 1 23 34))
         {:output 'a, :arglist ['b 'c], :body [1 23 34]})))

(def k (mapdater a [b c] (+ b c)))

(deftest mapdater-tests
  (is (= {:a 7 :b 3 :c 4} (k {:b 3 :c 4}))))
