(ns mljalfa.audit.rfile
  (:require
    [clojure.string :as cs]
    [clojure.java.io :as io]))


(defn fdir [fname] (str "resources/koaci/" fname ".html"))

(def dir "resources/klikbca/")

(defn sdir [fname] (str "resources/klikbca/" fname ".edn"))

(defprotocol IMetal
  (get-me [this])
  (get-you [this]))

(defrecord Metal [nama anjing]
  IMetal
  (get-me [this]
    (:nama this))
  (get-you [this]
    (:anjing this)))

