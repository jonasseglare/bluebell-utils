(ns epicea.vfibers.standard-test
  (:require [epicea.vfibers.standard :refer :all]
            [epicea.vfibers.core :as vfibers]
            [clojure.test :refer :all]))

(deftest primitive-test
  (is (vfibers/node? (primitive :double 9)))
  (is (vfibers/node? (primitive :double))))
