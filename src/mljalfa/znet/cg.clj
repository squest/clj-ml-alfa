(ns mljalfa.znet.cg
  (:require
    [couchbase-clj.client :as cc]
    [clojure.data.json :as json]
    [clojure.string :as cs]
    [clj-time.core :as t]))

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
       (map json/read-json)))

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
  [start-date]
  (loop [[sday strday] (next-day start-date) res {}]
    (if (= strday "20150630")
      res
      ))
  (loop [[x & xs] (open-log fname) res {}]
    (if x
      (recur xs (->> #(merge-with + % {(:content-id x) 1})
                     (update-in res [(:user-uuid x)])))
      res)))



