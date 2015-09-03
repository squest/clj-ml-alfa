(ns mljalfa.audit.klikbca
  (:require
    [clojure.string :as cs]
    [clojure-mail.core :refer :all]
    [clojure-mail.message :refer [read-message]]
    [mljalfa.audit.pepeng :refer [pepeng]]))

(def login
  (gen-store "sabda@zeniuseducation.com" pepeng))

(defn fdir [fname] (str "resources/klikbca/" fname ".edn"))

(defn uuid
  []
  (str (java.util.UUID/randomUUID)))

(defn get-date
  [date]
  (vector (subs date 4 7)
          (Integer/parseInt (subs date 26 28))))

(def mapi
  {"Jan" "01" "Feb" "02" "Mar" "03" "Apr" "04" "May" "05" "Jun" "06"
   "Jul" "07" "Aug" "08" "Sep" "09" "Oct" "10" "Nov" "11" "Dec" "12"})

(defn koaci
  []
  (let [mails (inbox login)
        ctr (atom 0)]
    (loop [[x & xs] mails]
      (let [message (read-message x)
            date (get-date (:date-sent message))]
        (when (not= ["Jun" 14] date)
          (when (re-find #"klikbca" (str (:from message)))
            (println (str "Yai got one " @ctr))
            (swap! ctr inc)
            (spit (fdir (str (get mapi (first date)) (second date) (uuid)))
                  (dissoc message :from)))
          (println date)
          (recur xs))))))




