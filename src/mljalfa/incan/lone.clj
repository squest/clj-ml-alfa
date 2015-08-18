(ns mljalfa.incan.lone
  (:require
    [incanter.core :as ic]
    [incanter.charts :as ch]
    [incanter.io :as io]
    [incanter.datasets :as dt]
    [clojure.string :as cs]
    [incanter.stats :as is]))

(defn fdir
  [fname]
  (str "resources/practice/" fname))

(defn open-edn
  [fname]
  (->> (str fname ".edn") fdir slurp read-string))

(def sales-data
  (let [tmp (open-edn "sales")]
    (zipmap (keys tmp) (mapv (comp vec rest butlast) (vals tmp)))))

(defn monthly-traffic
  [fname]
  (->> (into {} (io/read-dataset (fdir fname)))
       :columns second
       (mapv #(cs/replace % #"," ""))
       (mapv #(Integer/parseInt %))
       (partition-all 30)
       (mapv (partial reduce +))
       (#(conj (vec (butlast (butlast %)))
               (+ (last %) (last (butlast %)))))))

(def traffic-data
  (zipmap [:2013 :2014 :2015]
          (map monthly-traffic ["dt13.csv" "dt14.csv" "dt15.csv"])))

(def traffic-sales
  (merge-with #(mapv vec (partition 2 (interleave %1 %2)))
              traffic-data
              sales-data))

(def tset (ic/to-dataset traffic-data))
(def sset (ic/to-dataset sales-data))
(def pvset (->> (for [x ["mpv13.csv" "mpv14.csv" "mpv15.csv"]]
                  (->> (io/read-dataset (fdir x))
                       :columns
                       second
                       (map #(cs/replace % #"," ""))
                       (map #(Integer/parseInt %))))
                (map vec)
                (zipmap [:2013 :2014 :2015])
                (ic/to-dataset)))

(def vidset
  (->> (for [x ["mvid13.csv" "mvid14.csv" "mvid15.csv"]]
         (->> (io/read-dataset (fdir x) :header true)
              :columns
              second
              (map #(cs/replace % #"," ""))
              (map #(Integer/parseInt %))))
       (map vec)
       (zipmap [:2013 :2014 :2015])
       (ic/to-dataset)))

(defn ts-chart
  [year which-data]
  (let [which ({:traffic  tset
                :pageview pvset
                :video    vidset} which-data)
        tdata (ic/sel which :cols year)
        sdata (ic/sel sset :cols year)
        lm-traffic (is/linear-model sdata tdata)]
    (doto (ch/scatter-plot
            tdata sdata
            :title (str which-data " against sales " year)
            :x-label (str which-data)
            :y-label "Sales"
            :legend true)
      (ch/add-lines tdata
                    (:fitted lm-traffic)
                    :series-label "Linear model")
      (ic/view))))

(defn annual-chart
  [which-data]
  (let [which ({:traffic  tset
                :pageview pvset
                :video    vidset} which-data)
        bahan (map #(vector (ic/sel sset :cols %1)
                            (ic/sel which :cols %1))
                   [:2013 :2014 :2015])
        lms (->> (map second bahan)
                 (map is/linear-model (map first bahan)))]
    (doto (ch/scatter-plot
            (apply concat (map second bahan))
            (apply concat (map first bahan))
            :title (str which-data " against sales ")
            :x-label (str which-data)
            :y-label "Sales"
            :legend true)
      (ch/add-lines (nth (map second bahan) 0)
                    (:fitted (nth lms 0))
                    :series-label "2013")
      (ch/add-lines (nth (map second bahan) 1)
                    (:fitted (nth lms 1))
                    :series-label "2014")
      (ch/add-lines (nth (map second bahan) 2)
                    (:fitted (nth lms 2))
                    :series-label "2015")
      (ic/view))))

(defn all-chart
  [which-data]
  (let [which ({:traffic  tset
                :pageview pvset
                :video    vidset} which-data)
        bahan (map #(vector (ic/sel sset :cols %1)
                            (ic/sel which :cols %1))
                   [:2013 :2014 :2015])
        lms (is/linear-model (apply concat (map first bahan))
                             (apply concat (map second bahan)))]
    (doto (ch/scatter-plot
            (apply concat (map second bahan))
            (apply concat (map first bahan))
            :title (str which-data " against sales ")
            :x-label (str which-data)
            :y-label "Sales"
            :legend true)
      (ch/add-lines (apply concat (map second bahan))
                    (:fitted lms)
                    :series-label "Linear model")
      (ic/view))))























