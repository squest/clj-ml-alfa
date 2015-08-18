(ns mljalfa.incan.two
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

(def sales
  {:2013 [35000 42000 59000 67000 64000 37000
          81000 79000 83000 85000 126000 88000]
   :2014 [70000 92000 117000 127000 144000 123000
          165000 176000 183000 182000 233000 202000]
   :2015 [118332 333290 308232 275578 286000 252000
          502000 429000 393000 297000 604000 193000]})

(defn read-new
  [fname]
  (->> (open-csv fname)
       cs/split-lines
       (drop 7)
       (mapv #(cs/split % #"\""))
       butlast
       (mapv rest)
       (mapv #(let [[a b] %]
               (int (* (Integer/parseInt (cs/replace a #"," ""))
                       (->> (take 6 b)
                            rest
                            (apply str)
                            (java.lang.Double.))
                       0.01))))))

(defn clean-data
  "Dimension [:session :pageview :newsession :sales]"
  [file-type]
  (let [read-raw (fn [fname]
                   (->> (open-csv fname)
                        cs/split-lines
                        (drop 7)
                        (mapv #(cs/split % #"\""))
                        (map second)
                        (map #(cs/replace % #"," ""))
                        (mapv #(Integer/parseInt %))
                        butlast vec))
        koko (mapv read-new ["mnew13" "mnew14" "mnew15"])
        acum (for [[a b c d]
                   (mapv #(vector
                           (keyword (str "20" %))
                           (str "ms" %)
                           (str "mpv" %)
                           %2)
                         [13 14 15]
                         koko)]
               (let [[x1 x2 x3 mo]
                     (conj (mapv read-raw [b c])
                           d
                           [:jul :aug :sep :oct :nov :dec
                            :jan :feb :mar :apr :may :jun])]
                 {:year  a
                  :datum (mapv #(hash-map :month %1
                                          :datum [%2 %3 %4]
                                          :sales %5)
                               mo x1 x2 x3 (a sales))}))]
    ({:edn (save-edn "monthly131415" (vec acum))
      :json (save-json "monthly131415" (vec acum))} file-type)))

(defn distance
  [a b]
  (reduce + (mapv #(let [sqr (- %2 %1)] (* sqr sqr)) a b)))


(defn prep-data-for-clustering
  [fname]
  (let [raw (open-edn fname)]
    (-> #(mapv (fn [x] (assoc x :year %1)) %2)
        (mapcat (map :year raw) (map :datum raw))
        vec)))

(defn prep-datum
  [mapi]
  (let [{:keys [datum sales]} mapi]
    (mapv #(int (* 1000 (/ sales %))) datum)))

(defn ndim-average
  [xs]
  (let [ctr (count xs)]
    (->> (reduce + (for [j xs] (nth j i)))
         (for [i (range (count (first xs)))])
         (mapv #(/ % ctr)))))

(defn normalise
  [xs n]
  (let [pukaw (ndim-average (mapv :vector xs))]
    (mapv #(mapv (fn [x y] (int (* n (/ x y)))) % pukaw) (mapv :vector xs))))

(def cluster-data
  (let [data (prep-data-for-clustering "monthly131415")
        bahan (-> #(assoc % :vector (prep-datum %))
                  (mapv data)
                  (normalise 1000))]
    (map #(assoc % :bahan %2) data bahan)))

(def training-data
  (let [fclass (fn [x]
                 (cond (#{:jun :nov} (:month %))
                       (assoc x :class :sepi)
                       (#{:jan :aug} (:month %))
                       (assoc x :class :rame)
                       :else nil))]
    (keep fclass cluster-data)))

(defn knn-classify
  [training-data xs k]
  (let [ctr (count xs)
        mps ()]))





























