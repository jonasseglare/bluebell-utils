(ns nettle.vfibers.standard-test
  (:require [nettle.vfibers.standard :refer :all]
            [nettle.vfibers.core :as vfibers]
            [clojure.test :refer :all]))

(deftest primitive-test
  (is (vfibers/node? (primitive :double 9)))
  (is (vfibers/node? (primitive :double))))
