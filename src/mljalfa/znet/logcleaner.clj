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

(defn user->member
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

(defn convert-kopet-data
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

(defn store-user-log-kopet
  [start-date end-date]
  (loop [[sday strday] (t/local-date 2014 7 1) res {}]
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












