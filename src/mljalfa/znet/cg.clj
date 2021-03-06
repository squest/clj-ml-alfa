(ns mljalfa.znet.cg
  (:require
    [couchbase-clj.client :as cc]
    [clojure.data.json :as json]
    [clojure.string :as cs]
    [clj-time.core :as t]
    [clojure.set :as cset]))

(def dir "/Users/questmac/public/db/click-logs/")
(def dir-kopet "/Users/questmac/public/db/resources/")

(defn next-day
  "return the vector of next-day in jdate format and next-day in string format"
  [jdate]
  (let [nday (t/plus jdate (t/days 1))]
    [nday (cs/replace (str nday) #"-" "")]))

(defn open-log
  "open log files"
  [fname]
  (->> (str dir fname)
       (slurp)
       (cs/split-lines)
       (filter #(pos? (count %)))
       (pmap json/read-json)))

(defn open-log-kopet
  "open log files"
  [fname]
  (->> (str dir-kopet fname)
       (slurp)
       (cs/split-lines)
       (filter #(pos? (count %)))
       (pmap json/read-json)))

(defn open-file
  [fname]
  (->> (str "resources/relevant/" fname ".edn")
       slurp read-string))

(defn open-raw
  [fname]
  (->> (str "resources/raws/" fname ".edn") slurp read-string))

(defonce cdb (-> {:bucket "znetroyalty"
                  :uris   ["http://127.0.0.1:8091/pools"]}
                 (cc/create-client)))

(defn cbkey
  [naon id]
  {:key (str naon "-" id)})

(defn cbquery
  "Querying couchbase"
  ([qgroup view]
   (->> (cc/query cdb qgroup view {:include-docs true})
        (map cc/view-doc-json)))
  ([qgroup view opt]
   (->> (merge opt {:include-docs true})
        (cc/query cdb qgroup view)
        (map cc/view-doc-json))))

(defn store-user-log
  [start-date end-date]
  (loop [[sday strday] (next-day start-date) res {}]
    (if (= strday end-date)
      (do (spit "resources/avi-data.edn" res)
          (->> (take 100 res)
               (into {})))
      (let [resi (time (loop [[x & xs] (open-log (str "click-log-" strday)) resi {}]
                         (if x
                           (recur xs (->> #(merge-with + % {(:content-id x) 1})
                                          (update-in resi [(:user-uuid x)])))
                           resi)))]
        (do (println strday)
            (recur (next-day sday) (merge-with #(merge-with + %1 %2) res resi)))))))

(defn store-user-log-kopet
  [start-date end-date]
  (loop [[sday strday] (next-day start-date) res {}]
    (if (= strday end-date)
      (do (spit "resources/kopet-data.edn" res)
          (->> (take 100 res)
               (into {})))
      (let [resi (time (loop [[x & xs] (open-log-kopet (str "click-log-" strday)) resi {}]
                         (if x
                           (recur xs (->> #(merge-with + % {(first (:content-id x)) 1})
                                          (update-in resi [(:user-id x)])))
                           resi)))]
        (do (println strday)
            (recur (next-day sday) (merge-with #(merge-with + %1 %2) res resi)))))))

(defn get-top-parent
  [content-id]
  (let [{:keys [cg-id]} (cc/get-json cdb (str "content-" content-id))]
    (->> (cc/get-json cdb (str "content-group-" cg-id))
         :parents
         (filter #(#{1 2 3} (:id %)))
         first :id)))



(defn store-user-cg
  "Store the data in fname into cg form"
  [fname]
  (let [data (read-string (slurp "resources/avi-data.edn"))]
    (loop [[[ku vu] & xs] (seq data) res []]
      (if ku
        (let [comcom (distinct (keys vu))
              com2 (zipmap comcom (map get-top-parent comcom))
              tmp (loop [[[k v] & xxs] (seq vu) resi {}]
                    (if k
                      (recur xxs (merge-with + resi {(com2 k) v}))
                      resi))]
          (recur xs (conj res {ku tmp})))
        (do (spit (str "resources/" fname ".edn") res)
            (take 50 res))))))

(defn classify-based-on-video-access
  [fname]
  (let [raw (->> (slurp (str "resources/" fname ".edn"))
                 read-string)]
    (->> (map #(dissoc (first (vals %)) nil) raw)
         (remove empty?)
         (map #(apply max-key second %))
         (group-by first)
         (map #(vector (key %)
                       (count (val %))))
         (into {}))))

(defn user->member
  [fname]
  (let [raw (read-string (slurp "resources/avi-data.edn"))
        mapping (read-string (slurp "resources/lala.edn"))
        sraw (set (keys raw))]
    (loop [[[k v] & xs] (seq mapping) res {} i 0]
      (if k
        (do (when (== 0 (rem i 10000))
              (println i))
            (recur xs (merge res {(first (keep sraw k)) v}) (inc i)))
        (do (spit (str "resources/" fname ".edn") res)
            (take 20 res))))))

(defn member-base
  [fname]
  (let [raw (read-string (slurp "resources/selected.edn"))
        mapping (read-string (slurp "resources/convert.edn"))]
    (loop [[x & xs] raw res []]
      (if x
        (recur xs (conj res {(mapping (first (keys x))) (first (vals x))}))
        (do (spit (str "resources/" fname ".edn") res)
            (take 20 res))))))

(defn cg-sieve
  "assigning parents to cg"
  [fname]
  (let [refs (into-array (vec (repeat 2000 [])))
        raw (->> "resources/content-group-tree-path.edn"
                 slurp read-string)]
    (loop [[{:keys [ancestor descendant length]} & vs] raw]
      (if ancestor
        (do (->> {:id ancestor :level length}
                 (conj (aget refs descendant))
                 (aset refs descendant))
            (recur vs))
        (let [content (->> (into [] refs)
                           (map-indexed #(hash-map :id %1
                                                   :parents %2))
                           (remove #(empty? (:parents %)))
                           (into []))]
          (do (spit (str "resources/" fname ".edn") content)
              (take 20 content)))))))

(def cg-from-file (->> "resources/content-group.edn"
                       slurp read-string))

(defn update-cg
  "Update content-group from file"
  []
  (let [cgs (into-array (vec (repeat 2000 {})))]
    (do (loop [[v & vs] cg-from-file]
          (when v
            (aset cgs (:id v) v)
            (recur vs)))
        (loop [[v & vs] (->> "resources/cg-parents.edn"
                             slurp read-string)]
          (when v
            (aset cgs (:id v) (merge (aget cgs (:id v)) v))
            (recur vs)))
        (let [content (->> (into [] cgs)
                           (remove empty?)
                           (into []))]
          (spit "resources/updated-cg.edn" content)
          (take 20 content)))))

(defn update-cg-in-db
  "Update the content-group data in the db"
  []
  (loop [[v & vs] (open-raw "updated-cg")]
    (when v
      (let [id (:id v)]
        (if-let [from-db (cc/get-json cdb (:key (cbkey "content-group" id)))]
          (cc/set-json cdb (keyword (:key (cbkey "content-group" id)))
                       (merge from-db v))
          (cc/set-json cdb (keyword (:key (cbkey "content-group" id)))
                       (merge v {:ctype    "content-group"
                                 :contents []}))))
      (println (:id v))
      (recur vs))))

(defn update-content-in-db
  []
  (let [raw (->> "resources/content.edn" slurp read-string)]
    (loop [[v & vs] raw]
      (when v
        (let [id (:id v)]
          (if-let [from-db (cc/get-json cdb (:key (cbkey "content" id)))]
            (cc/get-json cdb (keyword (:key (cbkey "content" id)))
                         (merge from-db v))
            (cc/get-json cdb (keyword (:key (cbkey "content" id)))
                         (merge v {:ctype  "content"
                                   :tutors []
                                   :hits   {}}))))
        (println (:id v))
        (recur vs)))))

(defn convert-avi-data
  [fname]
  (let [convert (-> "resources/converts.edn" slurp read-string)]
    (->> "resources/avi-data.edn"
         slurp read-string
         (keep #(if-let [k (convert (key %))]
                 {k (val %)}))
         (into [])
         (spit (str "resources/" fname ".edn")))))

(defn convert-kopet-data
  []
  (let [raw (open-file "kopet-data")]
    (loop [[[k v] & vs] (seq raw) res []]
      (if k
        (recur vs (conj res {k (->> (map #(vector (str (key %)) (val %)) v)
                                    (into {}))}))
        (do (spit "resources/relevant/kopet-data-bagus.edn" res)
            (take 20 res))))))

(defn agglomerate-logs
  "Agglomerating logs from two log files"
  [fname]
  (let [raw1 (reduce merge (open-file "kopet-data-bagus"))
        raw2 (reduce merge (open-file "avi-data-member"))
        result (merge-with (partial merge-with +) raw1 raw2)]
    (spit (str "resources/relevant/" fname ".edn") result)
    (take 50 result)))



























