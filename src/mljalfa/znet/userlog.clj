(ns mljalfa.znet.userlog
  (:require
    [couchbase-clj.client :as cc]
    [clojure.data.json :as json]
    [clojure.string :as cs]
    [clj-time.core :as t]
    [clojure.set :as cset]
    [mljalfa.znet.couch :refer [open-file cbkey cdb cbquery]]))

(def dir "resources/relevant/")

(defn fdir [fname] (str dir fname ".edn"))

(defn open-raw
  [fname]
  (->> (str "resources/raws/" fname ".edn") slurp read-string))

(defn add-new-content
  []
  (let [raw (open-raw "content")]
    (loop [[v & vs] raw]
      (when v
        (let [id (:id v)]
          (if-let [from-db (cc/get-json cdb (:key (cbkey "content" id)))]
            (cc/set-json cdb (:key (cbkey "content" id))
                         (merge from-db (assoc v :ctype "content")))
            (cc/set-json cdb (:key (cbkey "content" id))
                         (assoc v :ctype "content" :tutors []))))
        (recur vs)))))

(defn convert-to-cg
  [fname]
  (let [raw (open-file "total")]
    (loop [[[k v] & vs] (seq raw) res []]
      (if k
        (let [values (map #(vector (key %) (val %)) v)
              new-val (loop [[[kk vv] & vvs] values res {}]
                        (if kk
                          (if-let [pom (cc/get-json cdb (:key (cbkey "content" kk)))]
                            (recur vvs (->> {(:cg-id pom) vv}
                                            (merge-with + res)))
                            (recur vvs (merge-with + res {nil vv})))
                          res))]
          (println k)
          (recur vs (conj res {k new-val})))
        (do (spit (fdir fname) res)
            (take 20 res))))))

(defn get-parent-level1
  [cg-id]
  (->> (cc/get-json cdb (str "content-group-" cg-id))
       :parents
       (filter #(#{41 42 43 44 45 639 640} (:id %)))
       first :id))

(defn convert-to-level1
  [fname]
  (let [raw (reduce merge (open-file "total-in-cg"))]
    (loop [[[k v] & vs] (seq raw) res (transient {})]
      (if k
        (let [values (map #(vector (key %) (val %)) v)
              new-val (loop [[[kk vv] & vvs] values res {}]
                        (if kk
                          (if-let [pom (get-top-parent kk)]
                            (recur vvs (->> {pom vv}
                                            (merge-with + res)))
                            (recur vvs (merge-with + res {nil vv})))
                          res))]
          (println k)
          (recur vs (assoc! res k new-val)))
        (let [resi (persistent! res)]
          (spit (fdir fname) resi)
          (take 10 resi))))))

(defn convert-to-level2
  [fname]
  (let [raw (open-file "users-sma-log")]
    (loop [[[k v] & vs] (seq raw) res (transient {})]
      (if k
        (let [values (map #(vector (key %) (val %)) v)
              new-val (loop [[[kk vv] & vvs] values res {}]
                        (if kk
                          (if-let [pom (get-parent-level1 kk)]
                            (recur vvs (->> {pom vv}
                                            (merge-with + res)))
                            (recur vvs (merge-with + res {nil vv})))
                          res))]
          (println k)
          (recur vs (assoc! res k new-val)))
        (let [resi (persistent! res)]
          (spit (fdir fname) resi)
          (take 10 resi))))))

(defn active-users
  [fname n]
  (let [raw (open-file "level1-notempty")]
    (->> (filter #(>= (reduce + (vals (val %))) n) raw)
         (into {})
         (spit (fdir (str fname n))))))

(defn max-by
  [f xs]
  (if (empty? xs)
    xs
    (loop [[x & xxs] (seq xs) cur x fik (f x)]
      (if x
        (let [tmp (f x)]
          (if (> tmp fik)
            (recur xxs x tmp)
            (recur xxs cur fik)))
        cur))))

(defn classify-level1
  [which-file]
  (let [raw (open-file which-file)]
    (->> raw
         (group-by #(first (max-by val (val %))))
         (map #(vector (key %) (count (val %))))
         (into {})
         ((juxt (partial spit (fdir "classification")) println)))))

(defn classify-sma-rest
  [which-file]
  (let [raw (open-file which-file)
        fsep (fn [[k v]]
               (let [sma (v 3 0) rests (reduce + (vals (dissoc v 3)))]
                 {:user k :datum [sma rests]}))]
    (->> (mapv fsep raw)
         ((juxt (partial spit (fdir "class-sma-rest")) println)))))















































