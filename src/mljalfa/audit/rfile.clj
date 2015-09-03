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

(defn in-range?
  [fname]
  (let [buli (set (map str
                       (concat (map #(str "0" %) (range 7 10))
                               (map str [10 11 12])
                               (map #(str "0" %) (range 1 7)))
                       (concat (repeat 6 14) (repeat 6 15))))]
    (some #{(subs fname 18 22)} buli)))

(defn produce
  [html]
  (let [ctr (atom 0)]
    (->> (sequence
           (comp (map str)
                 (filter in-range?))
           (drop 2 (file-seq (io/as-file dir))))
         (sort-by #(str (subs % 20 22) (subs % 18 20)))
         (keep #(try (do (->> % slurp read-string :body :body)
                         (println @ctr)
                         (swap! ctr inc))
                     (catch Exception e)))
         (reduce str)
         (spit (fdir html)))))

(def soto
  (->> (slurp "https://projecteuler.net/problem=11")
       cs/split-lines
       (drop 56)
       (map #(cs/replace % #"<br />" ""))
       (map #(cs/replace % #"<span style=\"color:#ff0000;\">" ""))
       (map #(cs/replace % #"<b>" ""))
       (map #(cs/replace % #"</b>" ""))
       (map #(cs/replace % #"</p>" ""))
       (map #(cs/replace % #"</span>" ""))
       (map #(cs/split % #" "))
       (take 20)
       (mapv (partial mapv bigint))
       (mapv (partial mapv int))))



























