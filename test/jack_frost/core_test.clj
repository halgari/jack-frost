(ns jack-frost.core-test
  (:require [clojure.test :refer :all]
            [jack-frost.core :refer :all]
            [jack-frost.data :as data]))

(deftest main-test
  (is (= data/data
         (thaw (freeze data/data)))))
