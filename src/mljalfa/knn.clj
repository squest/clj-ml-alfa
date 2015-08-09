(ns mljalfa.knn
  (:require
    [incanter.stats :as stat]))

(defn sqr [x] (* x x))

(defn distance
  [p1 p2]
  (reduce + (map #(sqr (- %1 %2)) p1 p2)))

(defn gen-people
  [^long n]
  (let [fheights (->> (stat/sample-normal n :mean 155 :sd 6)
                      (map int))
        fweights (->> (stat/sample-normal n :mean 46 :sd 4)
                      (map int))
        fhairs (->> (stat/sample-normal n :mean 30 :sd 4)
                    (map int))
        mheights (->> (stat/sample-normal n :mean 165 :sd 6)
                      (map int))
        mweights (->> (stat/sample-normal n :mean 63 :sd 6)
                      (map int))
        mhairs (->> (stat/sample-normal n :mean 15 :sd 3))]
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
    {:data result
     :summary {:success success :error error}} ))

