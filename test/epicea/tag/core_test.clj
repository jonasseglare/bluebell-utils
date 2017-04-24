(ns epicea.tag.core-test
  (:require [clojure.test :refer :all]
            [epicea.tag.core :refer :all] :reload-all))

(deftest tags
  (is (tagged? :success [:success 3]))
  (is (not (tagged? :success [:failure 3])))
  (is (= 3 (value [:success 3])))
  (is (= [:success 3] (tag :success 3)))
  (is ((tagged? :success) [:success 3]))
  (is (not ((tagged? :success) [:failure 3])))
  (is (= [:success 3] ((tag :success) 3))))
