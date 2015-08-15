(ns mljalfa.incan.one
  (:use [incanter core datasets])
  (:require [incanter.stats :as stat]))

(defonce iris (get-dataset :iris))
(defonce us-arrests (get-dataset :us-arrests))


(defn fdir [fname] (str "resources/relevant/" fname ".edn"))

(defn open
  [fname]
  (->> (str "resources/relevant/" fname ".edn")
       slurp read-string))

(def users-sma
  (->> (open "users-sma-datum")
       (map #(let [[a b c d] (:datum %)]
              {:sbmptn a :sma12&UN b :sma10 c :sma11 d}))
       (to-dataset)))



