(ns mljalfa.finance.voucher
  (:require
    [clojure.string :as cs]
    [clojure.set :as cset]
    [cheshire.core :as js]
    [incanter.core :as i]
    [incanter.stats :as s]
    [incanter.io :as io]
    [incanter.datasets :as ds]
    [incanter.charts :as c]
    [incanter.optimize :as o]))

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

(defn open-json
  [fname]
  (->> (str (fdir fname) ".json")
       slurp
       (js/parse-string)))

(defn parse-csv
  [fname dp]
  (->> (open-csv fname)
       cs/split-lines
       (drop dp)
       (map #(cs/replace % #"," ""))
       (map #(cs/split % #"\""))
       (mapv second)
       (mapv #(Integer/parseInt %))
       (take 12)
       vec))



(defn add-dim
  [fname xs]
  (let [raw (open-json fname)]
    (->> (mapv #(update-in % ["datum"] (fn [x] (conj x %2)))
               raw xs)
         (save-json fname))))


(defn interesting
  [mapi]
  (let [{:keys [month year]} mapi]
    (if (<= 2012 year 2015)
      (if (= year 2012)
        (if (>= month 7)
          {:month (- (+ month (* 12 (- year 2012))) 6)}
          {})
        {:month (- (+ month (* 12 (- year 2012))) 6)})
      {})))

(defn good-date
  [popo]
  (let [asshole (:timestamp popo)
        [a b] (-> (java.text.SimpleDateFormat. "MM yyyy")
                  (.format asshole)
                  (cs/split #" "))
        mapi (zipmap (map #(if (< % 10)
                            (str 0 %)
                            (str %))
                          (range 1 13))
                     (range 1 13))]
    (interesting
      {:month (mapi a 0)
       :year  (Integer/parseInt b)})))

(defn process-voucher
  [mapi]
  (let [map1 (merge mapi (good-date mapi))
        pompom (quot (:value map1 0) 30)
        mupi {1 150 2 200 3 200 4 200 5 300 6 300 9 375 12 400}]
    (if (pos? pompom)
      (assoc (dissoc map1 :timestamp :memberid :voucherid)
        :value pompom :revenue (quot (mupi pompom 0) pompom))
      {})))

(defn process-all-voucher
  [target]
  (let [refs (int-array 50 0)]
    (loop [[x & xs] (->> (open-edn "voucher")
                         (map process-voucher)
                         (filter #(:month % false)))]
      (when x
        (let [{:keys [month revenue value]} x]
          (doseq [i (range month (+ month value))]
            (aset refs i (+ revenue (aget refs i)))))
        (recur xs)))
    (->> (into [] refs)
         rest
         (take 36)
         (mapv vector (range 1 37))
         (save-edn target))))

(defn member-grouping
  [fname]
  (let [raw (open-edn fname)
        refs (make-array clojure.lang.PersistentArrayMap 360000)]
    (doseq [{:keys [value memberid]} raw]
      (aset refs memberid
            (merge-with + (aget refs memberid)
                        {:value value :activation 1})))
    (remove empty? (into [] refs))))

(defn process-duration
  [fname]
  (let [raw (->> (open-csv fname)
                 (cs/split-lines)
                 (remove #(< (count %) 4))
                 (drop-while #(not= "0000" (subs % 0 4)))
                 (mapv #(cs/split % #","))
                 (mapv second)
                 (mapv #(cs/split % #":"))
                 (mapv rest)
                 butlast vec)
        mins (zipmap (map #(if (< % 10) (str "0" %) (str %)) (range 0 61))
                     (map (partial * 60) (range 0 61)))
        secs (zipmap (map #(if (< % 10) (str "0" %) (str %)) (range 0 61))
                     (range 0 61))]
    (mapv #(let [[a b] %] (+ (mins a) (secs b))) raw)))

(defn normalise-complete
  []
  (let [wateva (open-json "complete")
        base (map #(zipmap (map keyword (keys %)) (vals %)) wateva)
        durations (open-edn "duration")
        result (mapv #(assoc % :duration %2) base durations)]
    (save-json "complete" result)
    (save-edn "complete" result)))

(defn add-duration-session
  [fname]
  (let [base (open-edn fname)
        result (mapv #(assoc (dissoc % :total-duration)
                       :totalduration
                       (int (/ (* (:session %) (:duration %)) 1000)))
                     base)]
    (doseq [f [save-edn save-json]]
      (f fname result))))

;; [:session :pageviews :newsession :blogviews :users :seo :direct]

(def incan-complete
  (i/to-dataset (open-edn "complete")))

(def revenues (i/sel incan-complete :cols :revenue))
(def seos (i/sel incan-complete :cols :seo))
(def durations (i/sel incan-complete :cols :totalduration))
(def pageviews (i/sel incan-complete :cols :pageview))
(def sessions (i/sel incan-complete :cols :session))

(def f-seo
  (s/linear-model revenues seos))

(def f-duration
  (s/linear-model revenues durations))

(def f-pageview
  (s/linear-model revenues pageviews))

(def f-session
  (s/linear-model revenues sessions))

(defn charts
  [which]
  (let [fmap {"SEO" f-seo
              "Durations" f-duration
              "PageViews" f-pageview
              "Sessions" f-session}
        dmap {"SEO" seos
              "Durations" durations
              "PageViews" pageviews
              "Sessions" sessions}]
    (doto (c/scatter-plot (dmap which)
                          revenues
                          :title (str which " vs Revenue")
                          :x-label which
                          :y-label "Revenue"
                          :legend true)
      (c/add-lines (dmap which) (:fitted (fmap which))
                   :series-label "Linear model")
      i/view)))

(defn add-seoduration
  [fname]
  (let [raw (open-edn fname)
        data (->> (mapcat process-duration ["seodur14" "seodur15"])
                  (concat (repeat 12 0)))
        result (mapv #(assoc %
                       :seoduration
                       (int (/ (* %2 (:seo %)) 200)))
                     raw data)]
    (doseq [f [save-edn save-json]]
      (f fname result))))













