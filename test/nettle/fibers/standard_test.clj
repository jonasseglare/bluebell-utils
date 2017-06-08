(ns nettle.fibers.standard-test
  (:require [nettle.fibers.standard :refer :all]
            [nettle.fibers.core :as fibers]
            [clojure.test :refer :all]))

(deftest primitive-test
  (is (fibers/node? (primitive :double 9)))
  (is (fibers/node? (primitive :double))))
