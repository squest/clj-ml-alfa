(ns mljalfa.finance.voucher
  (:require
    [clojure.string :as cs]
    [clojure.set :as cset]
    [cheshire.core :as js]))

(defn fdir
  [fname]
  (str "resources/aug18/" fname))

(defn save-edn
  [fname data]
  (spit (str (fdir fname) ".edn") data))

(defn save-json
  [fname data]
  (spit (str (fdir fname) ".json")
        (js/generate-string data)))

(defn open-edn
  [fname]
  (->> (str (fdir fname) ".edn") slurp read-string))

(defn open-csv
  [fname]
  (->> (str (fdir fname) ".csv") slurp))

(defn good-date
  [asshole]
  (let [[a b] (-> (java.text.SimpleDateFormat. "MM yyyy")
                  (.format asshole)
                  (cs/split #" "))
        mapi (zipmap (map #(if (< % 10)
                            (str 0 %)
                            (str %))
                          (range 1 13))
                     (range 1 13))]
    {:month (mapi a) :year (Integer/parseInt b)}))
