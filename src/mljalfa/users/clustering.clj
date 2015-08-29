(ns mljalfa.users.clustering
  (:require
    [cheshire.core :as cc]
    [clojurewerkz.statistiker.clustering.kmeans :as kmeans]
    [clojurewerkz.statistiker.clustering.dbscan :as dbscan]))


(defn fdir
  [fname ext]
  (str "resources/aug28/" fname "." ext))

(defn open-edn
  [fname]
  (->> (fdir fname "edn")
       slurp read-string))

(defn save-edn
  [fname data]
  (spit (fdir fname "edn") data))

(defn open-json
  [fname]
  (->> (fdir fname "json")
       slurp cc/parse-string))

(defn save-json
  [fname data]
  (spit (fdir fname "json") (cc/generate-string data)))

(defn get-level-one
  [target]
  (let [mapi (open-edn "lookup-parents")
        data (rest (open-edn "members-in-cg"))]
    (mapv #(-> (dissoc % :contents)
               (merge (->> (group-by (fn [x] (:level-1 (mapi (key x)))) (:contents %))
                           (mapv (fn [x] {(key x) (reduce + (map second (val x)))}))
                           (reduce merge)))
               ((partial merge-with +) {3 0 2 0 1 0}))
          data)))

(defn normalise
  [xs]
  (let [[a b c] (for [i [1 2 3]]
                  (->> (for [m xs] (get m i))
                       (apply max)))
        sd (int (/ c a))
        smp (int (/ c b))]
    (mapv #(assoc %
            1 (* sd (get % 1))
            2 (* smp (get % 2))) xs)))

(defn dreduce
  [xs]
  (mapv #(assoc (dissoc % 1 2 3)
          :sma (get % 3)
          :sdsmp (+ (get % 1) (get % 2)))
        xs))

(def training-data
  (let [tmp (take 100 (shuffle (open-edn "weighted-members")))
        mapi {1 :sd 2 :smp 3 :sma}]
    (mapv #(assoc %
            :class
            (->> (apply max-key val %)
                 key
                 mapi))
          tmp)))

(defn distance
  [p1 p2]
  (let [sqr #(* % %)]
    (reduce + (map #(sqr (- %1 %2)) p1 p2))))




















