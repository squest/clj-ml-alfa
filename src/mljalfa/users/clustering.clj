(ns mljalfa.users.clustering
  (:require [cheshire.core :as cc]))


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
