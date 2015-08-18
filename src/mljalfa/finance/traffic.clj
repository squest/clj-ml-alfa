(ns mljalfa.finance.traffic
  (:require
    [cheshire.core :as js]
    [clojure.string :as cs]
    [incanter.core :as ic]
    [incanter.charts :as ch]
    [incanter.io :as io]))

(defn fdir
  [fname]
  (str "resources/sales/" fname))

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

(defn extract-csv
  [fname]
  (->> (open-csv fname)
       (cs/split-lines)
       (map #(cs/split % #"\""))
       (map second)
       (map #(cs/replace % #"," ""))
       (mapv #(Integer/parseInt %))))

(def page-views
  (let [fnames ["mpv13" "mpv14" "mpv15"]
        toms [:2013 :2014 :2015]]
    (mapv #(hash-map :year %1
                     :datum (extract-csv %2))
          toms fnames)))

(def sales-data (open-edn "sales"))
(def msales-data (->> sales-data
                      (mapv #(hash-map :year (key %)
                                       :data (vec (rest (butlast (val %))))))))
(def traffic-data (open-edn "traffic"))
(def traffic-sales
  (let [sales (map sales-data [:2013 :2014 :2015])
        traffic (map traffic-data [:2013 :2014 :2015])
        picnic [:2013 :2014 :2015]]
    (-> #(hash-map %1 (mapv vector %2 %3))
        (map picnic traffic sales)
        ((partial reduce merge)))))


