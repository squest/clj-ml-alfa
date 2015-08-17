(ns mljalfa.znet.learning.snmptn
  (:require
    [clojure.data.json :as json]
    [cheshire.core :as js]
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

(comment
  (defn kelas12?
    [user-data]
    (let [datum (assoc (:datum user-data) -1 1)
          pompom (apply max-key val datum)]
      (if (cg-smas (first pompom)) (> (reduce + (vals datum)) 50) false)))

  (defn count-lulus-snmptn
    [pre-source post-source]
    (let [pre-data (open-file pre-source)
          post-data (sequence
                      (map :memberid)
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
      (if (cg-smas (first pompom)) (> (datum 45 0) 1) false)))

  (defn count-lulus-sbmptn
    [pre-source post-source]
    (let [pre-data (open-file pre-source)
          post-data (sequence
                      (map :memberid)
                      (open-file post-source))
          bersbmptn (->> (sequence
                           (comp (filter bersbmptn?)
                                 (map :memberid))
                           pre-data)
                         set)]
      [(count bersbmptn) (->> (keep bersbmptn post-data) count)])))

(defn process-logs
  [source-file target-file]
  (let [raw (open-file source-file)
        get-cg (fn [x] (->> (ckey "content" x)
                            (cc/get-json cdb)
                            :cg-id))
        contents (->> (vals raw)
                      (mapcat keys)
                      distinct)
        cc->cg (zipmap contents (pmap get-cg contents))
        acum-cg (fn [x] (->> (map #(hash-map (cc->cg (key %)) (val %)) x)
                             (reduce #(merge-with + %1 %2) {})))]
    (->> (mapv #(hash-map :memberid (key %)
                          :datum (acum-cg (val %))) raw)
         (spit (fdir target-file)))))

(defn process-logs-to-parents
  [source-file target-file]
  (let [raw (open-file source-file)
        get-cg (fn [x] (->> (ckey "content-group" x)
                            (cc/get-json cdb)
                            :parents
                            (sort-by :level >)
                            second :id))
        cgs (->> (map :datum raw)
                 (mapcat keys)
                 distinct)
        cg->parent (zipmap cgs (pmap get-cg cgs))
        acum-parent (fn [x] (->> (map #(hash-map (cg->parent (key %)) (val %)) x)
                                 (reduce #(merge-with + %1 %2) {})))]
    (->> (mapv #(hash-map :memberid (:memberid %)
                          :datum (acum-parent (:datum %))) raw)
         (spit (fdir target-file)))))

(defn max-by [f [x & xs]]
  (if xs
    (let [tmp (f x)
          tmprest (max-by f xs)]
      (if (> tmp (f tmprest)) x tmprest))
    x))

(defn top-parent-clustering
  [source-file target-file]
  (let [raw (open-file source-file)
        not-empty-data (let [tmp (remove #(empty? (:datum %)) raw)]
                         (println (count tmp)) tmp)
        top-parent (fn [x] (max-by val (seq (:datum x))))
        cg->class {41   :sma10KTSP
                   42   :sma11KTSP
                   43   :sma12UN
                   44   :sma12UN
                   45   :sma12Alumni
                   639  :sma10K13
                   640  :sma11K13
                   1028 :sma12+UN}]
    (->> not-empty-data
         (keep #(let [[k v] (top-parent %)
                      maxi (reduce + (vals (:datum %)))]
                 (when (cg-smas k)
                   (assoc % :class :sma
                            :subclass (cg->class k)
                            :maxi v
                            :total maxi))))
         (into [])
         (spit (fdir target-file)))))

(defn cluster-all
  []
  (doseq [x ["pre-snmptn-in-parent"
             "pre-pengumuman-sbmptn-in-parent"]]
    (println x)
    (time (top-parent-clustering x (str x "-filtered-sma")))
    (println (count (open-file (str x "-filtered-sma"))))))

(defn count-class
  [source-file tar mini maxi]
  (let [raw2 (open-file source-file)
        result (->> raw2
                    (filter #(<= mini (:total %) maxi))
                    (group-by :subclass)
                    (mapv #(hash-map :subclass (key %)
                                     :total (count (val %)))))
        total (reduce + (map :total result))
        result2 (mapv #(assoc % :percent (str (int (* 100 (/ (:total %) total))) "%"))
                      result)]
    (spit (fdir (str "report-sma-" tar "-" mini "-" maxi)) result2)
    (println result)))

(defn save-json
  [target xs]
  (spit (str "resources/logs/" target ".json")
        (js/generate-string xs)))

(defn get-sma-subclass
  [source-file target-file subclass]
  (->> (sequence
         (comp (map #(select-keys % [:subclass :memberid :total]))
               (filter #(and (<= 50 (:total % 0) 5000)
                             (= subclass (:subclass %))))
               (map :memberid))
         (open-file source-file))
       set
       (spit (fdir target-file))))

(defn filter-sma-subclass
  [source target subclass]
  (->> (open-file source)
       (filterv #(= subclass (:subclass %)))
       (filterv #(>= (:total % 0) 500))
       (spit (fdir target))))

(defn count-lulus
  [source-file filter-file tests]
  (let [mems (open-file source-file)
        members (->> mems
                     (map :memberid)
                     set)
        nonton (->> (open-file filter-file)
                    (filter #(>= (reduce + (vals (:datum %))) 50))
                    (map :memberid)
                    (keep members)
                    set)
        masih-nonton (count nonton)
        total (count members)
        result {:total-member         total
                :masih-nonton         masih-nonton
                :mungkin-karena-lulus (- total masih-nonton)}
        lulus? (fn [x] (if (nonton (:memberid x)) true false))]
    (spit (fdir (str "lulus-" tests "-report")) result)
    (->> (mapv #(assoc % :lulus (lulus? %)) mems)
         (spit (fdir "classified-lulus-sbmptn")))
    result))

(def name-smas
  '({:canonical-name "pelajaran-sma-kelas-10", :id 41}
     {:canonical-name "pelajaran-sma-kelas-11", :id 42}
     {:canonical-name "pelajaran-sma-kelas-12", :id 43}
     {:canonical-name "un-ujian-nasional-sma", :id 44}
     {:canonical-name "sbmptn-snmptn-simak-ui-ujian-mandiri", :id 45}
     {:canonical-name "pelajaran-sma-kelas-10-kurikulum-2013", :id 639}
     {:canonical-name "pelajaran-sma-kelas-11-kurikulum-2013", :id 640}
     {:canonical-name "pelajaran-sma-kelas-12-kurikulum-2013", :id 1028}))

