(ns mljalfa.core-test
  (:require [mljalfa.core :refer :all]
            [mljalfa.macros :refer :all]))

(def-test
  "Sieve eratos summation"
  142913828922
  (sum-sieve 2000000))

(deflet-test
  "Let testing sieve"
  [lim 2000000]
  142913828922
  (sum-sieve lim))



