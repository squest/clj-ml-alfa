(ns mljalfa.znet.classify
  (:require
    [clojure.string :as cs]
    [clojure.set :as cset]
    [mljalfa.znet.couch :refer [cbkey cdb cbquery]]
    [couchbase-clj.client :as cc]))

(def dir "resources/relevant/")

(defn fdir [fname] (str dir fname ".edn"))

(defn open
  [fname]
  (->> (fdir fname) slurp read-string))

(defn square [x] (* x x))

(defn distance
  [v1 v2]
  (->> (map #(square (- %1 %2)) v1 v2)
       (reduce +)))

(defn ndim-average
  [xs]
  (let [len (count (first xs))
        length (count xs)
        res (int-array len 0)]
    (loop [[x & xxs] xs]
      (when x
        (doseq [i (range len)]
          (aset res i (+ (aget res i) (nth x i))))
        (recur xxs)))
    (->> (into [] res)
         (mapv #(int (/ % 1.0 length))))))

(defn ndim-median
  [xs]
  (let [len (count (first xs))]
    (vec (for [i (range len)]
           (key (apply max-key val (frequencies (map #(nth % i) xs))))))))

(defn median
  [xs]
  (vec (key (apply max-key val (frequencies xs)))))

;; users sma 18331
;; sma x 2331, xi 2215, xii + alumni => 12410

(defn kmeans
  "k-means clustering with max-iter"
  [xs k max-iter]
  (let [len (count xs)
        takes (repeatedly k #(rand-int len))
        starts (->> (map #(nth xs %) takes)
                    (map #(hash-map :datum %2 :class %1) (range)))
        fdist (fn [p] (fn [p1] (distance (p1 :datum) (p :datum))))]
    (loop [i 0 data (mapv #(hash-map :datum %) xs) start starts restart []]
      (if (or (== i max-iter) (= start restart))
        data
        (let [next-data (mapv #(let [mini (apply min-key (fdist %) start)]
                                (assoc % :class (:class mini))) data)
              nstart (->> (group-by :class next-data)
                          (map #(hash-map :class (key %)
                                          :datum (ndim-average (map :datum (val %))))))]
          (do (println i)
              (println nstart)
              (recur (inc i) next-data nstart start)))))))

(defn kmedian
  "k-median clustering with max-iter"
  [xs k max-iter]
  (let [len (count xs)
        takes (repeatedly k #(rand-int len))
        starts (->> (map #(nth xs %) takes)
                    (map #(hash-map :datum %2 :class %1) (range)))
        fdist (fn [p] (fn [p1] (distance (p1 :datum) (p :datum))))]
    (loop [i 0 data (mapv #(hash-map :datum %) xs) start starts restart []]
      (if (or (== i max-iter) (= start restart))
        data
        (let [next-data (mapv #(let [mini (apply min-key (fdist %) start)]
                                (assoc % :class (:class mini))) data)
              nstart (->> (group-by :class next-data)
                          (map #(hash-map :class (key %)
                                          :datum (ndim-median (map :datum (val %))))))]
          (do (println i)
              (println nstart)
              (recur (inc i) next-data nstart start)))))))

(defn cluster
  [mini maxi]
  (time (->> (kmeans (->> (open "users-sma-datum")
                          (map #(let [[a b c d] (:datum %)]
                                 [(+ c d) a b]))
                          (filter #(and (every? (fn [x] (< x maxi)) %)
                                        (some (fn [x] (> x mini)) %)))) 3 50)
             (spit (fdir (str "class-users-sma-3dim-" mini "-" maxi))))))

(defn create-view
  [partname]
  (doseq [i (range 3)]
    (->> (open (str "class-users-sma-3dim-" partname))
         (filter #(= i (:class %)))
         (mapv :datum)
         (spit (fdir (str "class-users-sma-3dim-" partname "-view-" i))))))

(defn cluster2
  [partname]
  (let [data (map #((comp vec rest) (:datum %))
                  (open "users-sma-datum"))
        fclass (fn [x] (let [maxi (max (apply max x) 1)]
                         (mapv #(int (* 1000 (/ % maxi))) x)))]
    (->> (kmeans (mapv fclass data) 3 100)
         (spit (fdir (str "class-users-sma-3dim-" partname))))))

(defn cluster3
  [partname]
  (let [data (open "users-sma-datum")
        fclass (fn [x] (let [[a _] (->> (map-indexed #(vector %1 %2) x)
                                        (apply max-key second))]
                         {:datum x :class a}))]
    (->> (mapv #(fclass (vec (rest (:datum %)))) data)
         (spit (fdir (str "class-users-sma-3dim-" partname))))))























