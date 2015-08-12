(ns mljalfa.znet.classify
  (:require
    [clojure.string :as cs]
    [clojure.set :as cset]))

(def dir "resources/relevant/")

(defn fdir [fname] (str dir fname ".edn"))

(defn open-file
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
        res (int-array len 0)]
    (loop [[x & xxs] xs]
      (when x
        (doseq [i (range len)]
          (aset res i (+ (aget res i) (nth x i))))
        (recur xxs)))
    (->> (into [] res)
         (mapv #(int (/ % 1.0 len))))))

(defn kmeans
  "k-means clustering with max-iter"
  [xs k max-iter]
  (let [len (count xs)
        takes (repeatedly k #(rand-int len))
        starts (->> (map #(nth xs %) takes)
                    (map #(hash-map :datum %2 :class %1) (range)))
        fdist (fn [p] (fn [p1] (distance (p1 :datum) (p :datum))))]
    (loop [i 0 data (mapv #(hash-map :datum %) xs) start starts]
      (if (== i max-iter)
        (spit (fdir "cluster-1") data)
        (let [next-data (mapv (fn [x]
                               (let [mini (min-key (fdist x) start)]
                                 (assoc x :class (:class mini)))) data)
              nstart (->> (group-by :class next-data)
                          (map #(hash-map :class (key %)
                                          :datum (ndim-average (map :datum (val %))))))]
          (do (println i)
              (println nstart)
              (recur (inc i) next-data nstart)))))))