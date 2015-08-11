(ns mljalfa.znet.cg
  (:require
    [couchbase-clj.client :as cc]
    [clojure.data.json :as json]
    [clojure.string :as cs]
    [clj-time.core :as t]
    [clojure.set :as cset]))

(def dir "/Users/questmac/public/db/click-logs/")

(defn next-day
  [jdate]
  (let [nday (t/plus jdate (t/days 1))]
    [nday (cs/replace (str nday) #"-" "")]))

(defn open-log
  [fname]
  (->> (str dir fname)
       (slurp)
       (cs/split-lines)
       (filter #(pos? (count %)))
       (pmap json/read-json)))

(defonce cdb (-> {:bucket "znetroyalty"
                  :uris   ["http://127.0.0.1:8091/pools"]}
                 (cc/create-client)))

(defn cbkey
  [naon id]
  {:key (str naon "-" id)})

(defn cbquery
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

(defn classify
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
  (let [raw (read-string (slurp "resources/selected.edn"))
        mapping (read-string (slurp "resources/lala.edn"))
        sraw (set (map first (map keys raw)))]
    (loop [[[k v] & xs] (seq mapping) res {}]
      (if k
        (recur xs (merge res {(first (keep sraw k)) v}))
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

(defn what?)



