(ns mljalfa.znet.logcleaner
  (:require
    [clojure.data.json :as json]
    [clojure.string :as cs]
    [clj-time.core :as t]
    [clojure.set :as cset]))

(def dir "/Users/questmac/public/db/click-logs/")
(def dir-kopet "/Users/questmac/public/db/resources/")

(defn fdir [fname] (str "resources/logs/" fname ".edn"))

(defn open-file [fname]
  (->> (fdir fname) slurp read-string))

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
       (remove empty?)
       (pmap json/read-json)))

(defn open-log-kopet
  "open log files"
  [fname]
  (->> (str dir-kopet fname)
       (slurp)
       (cs/split-lines)
       (remove empty?)
       (pmap json/read-json)))

(defn adhoc-convert-member-data
  "Extract memberid and membername from all members in znet"
  [target-file]
  (->> (open-file "member")
       (mapv #(select-keys % [:memberid :membername]))
       (spit (fdir target-file))))

(defn adhoc-extract-users-from-log
  "Extract user-id from all users in avi style"
  [target-file]
  (->> (open-file "users")
       (mapv #(first (keys %)))
       (spit (fdir target-file))
       time))

(defn adhoc-user->member
  [target-file]
  (let [raw (open-file "set-avi-users")
        mapping (open-file "users-members-map")]
    (loop [[[k v] & xs] (seq mapping) res {} i 0]
      (if k
        (do (when (== 0 (rem i 10000))
              (println i))
            (recur xs (merge res {(first (keep raw k)) v}) (inc i)))
        (do (spit (fdir target-file) res)
            (take 20 res))))))

(defn adhoc-convert-kopet-data
  [ftemp]
  (let [ldir (fn [str-date]
               (str "resources/logs/bydate/" ftemp str-date ".edn"))]
    (loop [[sday strday] (next-day (t/local-date 2014 6 30))]
      (when (not= strday "20150130")
        (->> (open-log-kopet (str "click-log-" strday))
             (mapv #(hash-map :content-id (first (:content-id %))
                              :timestamp (apply str (take 10 (:timestamp %)))
                              :memberid (:user-id %)))
             (spit (ldir strday))
             time)
        (println strday)
        (recur (next-day sday))))))

(defn adhoc-convert-avi-data
  [ftemp]
  (let [ldir (fn [str-date]
               (str "resources/logs/bydate/" ftemp str-date ".edn"))
        user-map (open-file "user-member-map")]
    (loop [[sday strday] (next-day (t/local-date 2015 1 30))]
      (when (not= strday "20150801")
        (->> (open-log (str "click-log-" strday))
             (mapv #(hash-map :content-id (if-let [cid (:content-id %)]
                                            (Integer/parseInt cid) 0)
                              :timestamp (apply str (take 10 (:timestamp %)))
                              :memberid (user-map (:user-uuid %))))
             (spit (ldir strday))
             time)
        (println strday)
        (recur (next-day sday))))))













