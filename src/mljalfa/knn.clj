(ns mljalfa.knn
  (:require
    [incanter.stats :as stat]
    [incanter.charts :as chart]
    [incanter.core :refer [view]]))



(defn sqr [x] (* x x))

(defn distance
  [p1 p2]
  (reduce + (map #(sqr (- %1 %2)) p1 p2)))

(defn gen-people
  [^long n]
  (let [fheights (stat/sample-normal n :mean 155 :sd 6)
        fweights (stat/sample-normal n :mean 46 :sd 4)
        fhairs (stat/sample-normal n :mean 30 :sd 4)
        mheights (stat/sample-normal n :mean 165 :sd 6)
        mweights (stat/sample-normal n :mean 63 :sd 6)
        mhairs (stat/sample-normal n :mean 20 :sd 4)]
    (concat (->> (map vector (shuffle mheights) (shuffle mweights) (shuffle mhairs))
                 (map #(hash-map :class :male :datum %)))
            (->> (map vector (shuffle fheights) (shuffle fweights) (shuffle fhairs))
                 (map #(hash-map :class :female :datum %))))))

(defn classify
  "Classify one data xi"
  [xi train-data k]
  (->> train-data
       (map #(assoc % :distance (distance (:datum %) (:datum xi))))
       (sort-by :distance)
       (take k)
       (group-by :class)
       (map #(hash-map :class (key %)
                       :votes (count (val %))))
       (apply max-key :votes)
       :class
       (assoc xi :result)))

(defn classify-all
  "Classify all data"
  [reals-data train-data k]
  (let [result (map #(classify % train-data k) reals-data)
        success (count (filter #(= (:class %) (:result %)) result))
        error (- (count reals-data) success)]
    {:data    result
     :summary {:success success :error error}}))

