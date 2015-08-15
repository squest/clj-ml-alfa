(ns mljalfa.znet.learning.snmptn
  (:require
    [clojure.data.json :as json]
    [clojure.string :as cs]
    [clj-time.core :as t]
    [clojure.set :as cset]
    [mljalfa.znet.couch :refer [cdb cbkey cbquery]]
    [couchbase-clj.client :as cc])
  (:import (clojure.lang PersistentArrayMap PersistentHashMap)))

(defn fdir [fname] (str "resources/logs/" fname ".edn"))

(defn open-file [fname]
  (->> (fdir fname) slurp read-string))

(defn open-log [fname]
  (->> (fdir (str "bydate/" "log" fname)) slurp read-string))

(defn ckey [naon id] (:key (cbkey naon id)))

(defn next-day
  "return the vector of next-day in jdate format and next-day in string format"
  [jdate]
  (let [nday (t/plus jdate (t/days 1))]
    [nday (cs/replace (str nday) #"-" "")]))

(defn accum-snmptn-content-sucks
  "This function sucks, it doesn't work"
  [end-date]
  (let [member (make-array clojure.lang.PersistentArrayMap 330000)]
    (loop [[sday strday] (next-day (t/local-date 2014 6 30))]
      (if (= strday end-date)
        (let [tmp (->> (range 330000)
                       (keep-indexed #(when-let [v (aget member %)] {% v})))]
          (spit (fdir "pre-snmptn") tmp)
          (spit (fdir "pre-snmptn-json") (json/json-str tmp)))
        (do (time (loop [[x & xs] (open-log strday)]
                    (when x
                      (let [mid (:memberid x)]
                        (->> {(:content-id x) 1}
                             (merge-with + (aget member mid))
                             (aset member mid)))
                      (recur xs))))
            (println strday)
            (recur (next-day sday)))))))

(defn accum-snmptn-content
  [start-date end-date fname]
  (let [member (atom {})]
    (loop [[sday strday] (next-day start-date)]
      (if (= strday end-date)
        (let [tmp (dissoc @member nil 0)]
          (spit (fdir fname) tmp)
          (->> (json/json-str tmp)
               (spit (fdir (str fname "-json")))))
        (do (time (loop [[x & xs] (open-log strday)]
                    (when x
                      (let [mid (:memberid x)
                            vlue (@member mid)]
                        (swap! member #(->> {(:content-id x) 1}
                                            (merge-with + vlue)
                                            (assoc % mid))))
                      (recur xs))))
            (println strday)
            (recur (next-day sday)))))))


(defn convert-content->cg-in-users-log
  [source-file target-file]
  (let [data (open-file source-file)
        cgs (int-array 7000 -1)
        res (atom [])
        koko (atom 0)]
    (loop [i (int 0)]
      (when (< i 7000)
        (if-let [tmp (:cg-id (cc/get-json cdb (ckey "content" i)))]
          (do (aset cgs i tmp)
              (recur (+ i 1)))
          (do (println @koko i)
              (swap! koko inc)
              (recur (+ i 1))))))
    (loop [[[k v] & kvs] (seq data)]
      (if k
        (let [tmp (->> (dissoc v nil)
                       (map #(hash-map (aget cgs (key %)) (val %)))
                       (reduce #(merge-with + %1 %2) {}))]
          (swap! res conj {:memberid k :datum tmp})
          (recur kvs))
        (do (spit (fdir target-file) @res)
            (spit (fdir (str target-file "-json"))
                  (json/json-str @res)))))))

(def cg-smas
  (hash-set 41 42 43 44 45 639 640 1028))

(defn filtered-cg->level-one
  [source-file target-file toplevel]
  (let [data (open-file source-file)
        level ({1 "cg-sd" 2 "cg-smp" 3 "cg-sma"} toplevel)
        cg-set (open-file level)
        ctr (atom 1)
        res (atom [])]
    (loop [[[k v] & kvs] (seq data)]
      (if k
        (let [tmp (->> (dissoc v nil)
                       (keep #(when-let [pp (cg-set (key %))]
                               {(->> (cc/get-json cdb (ckey "content-group" pp))
                                     (:parents)
                                     (map :id)
                                     (keep cg-smas)
                                     first)
                                (val %)}))
                       (reduce #(merge-with + %1 %2) {}))]
          (swap! res conj {:memberid k :datum tmp})
          (when (zero? (rem @ctr 5000))
            (println @ctr))
          (swap! ctr inc)
          (recur kvs))
        (spit (fdir target-file) @res)))))

(defn convert-all-snmptn-sbmptn-data
  []
  (doseq [i ["pre-snmptn" "post-snmptn" "pre-pengumuman-sbmptn" "post-pengumuman-sbmptn"]]
    (filtered-cg->level-one i (str i "-filtered") 3)))

(defn kelas12?
  [user-data]
  (let [datum (assoc (:datum user-data) -1 1)
        pompom (apply max-key val datum)]
    (if (#{43 44 45} (first pompom)) (> (datum 43 0) 10) false)))

(defn count-lulus-snmptn
  [pre-source post-source]
  (let [pre-data (open-file pre-source)
        post-data (sequence
                    (comp (filter #(<= (reduce + (vals (:datum %))) 5))
                          (map :memberid))
                    (open-file post-source))
        kelas12 (->> (sequence
                       (comp (filter kelas12?)
                             (map :memberid))
                       pre-data)
                     set)]
    [(count kelas12) (->> (keep kelas12 post-data) count)]))

(defn bersbmptn?
  [user-data]
  (let [datum (assoc (:datum user-data) -1 1)
        pompom (apply max-key val datum)]
    (if (#{41 42 43 44 45} (first pompom)) (> (datum 45 0) 10) false)))

(defn count-lulus-sbmptn
  [pre-source post-source]
  (let [pre-data (open-file pre-source)
        post-data (sequence
                    (comp (filter #(<= (reduce + (vals (:datum %))) 5))
                          (map :memberid))
                    (open-file post-source))
        bersbmptn (->> (sequence
                         (comp (filter bersbmptn?)
                               (map :memberid))
                         pre-data)
                       set)]
    [(count bersbmptn) (->> (keep bersbmptn post-data) count)]))



(def name-smas
  '({:canonical-name "pelajaran-sma-kelas-10", :id 41}
     {:canonical-name "pelajaran-sma-kelas-11", :id 42}
     {:canonical-name "pelajaran-sma-kelas-12", :id 43}
     {:canonical-name "un-ujian-nasional-sma", :id 44}
     {:canonical-name "sbmptn-snmptn-simak-ui-ujian-mandiri", :id 45}
     {:canonical-name "pelajaran-sma-kelas-10-kurikulum-2013", :id 639}
     {:canonical-name "pelajaran-sma-kelas-11-kurikulum-2013", :id 640}
     {:canonical-name "pelajaran-sma-kelas-12-kurikulum-2013", :id 1028}))

