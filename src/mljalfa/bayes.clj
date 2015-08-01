(ns mljalfa.bayes
  (:require
    [incanter.stats :as is]
    [incanter.core :as ic :refer [view]]
    [incanter.charts :as ch :refer [xy-plot scatter-plot add-lines]]
    [clatrix.core :as cl]
    [clojure.core.matrix :refer :all]
    [clojure.core.matrix.operators :as M]))

(defmacro for-var
  [lsts restype]
  (let [len (count lsts)
        poms (vec (repeatedly len gensym))
        binds (vec (mapcat vector poms lsts))]
    `(for ~binds (into ~restype ~poms))))

(def gen-data
  (fn [rst]
    (shuffle (for-var rst #{}))))




