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
        mheights (->> (stat/sample-normal n :mean 165 :sd 6)
                      (map int))
        mweights (->> (stat/sample-normal n :mean 63 :sd 6)
                      (map int))]
    (concat (->> (map vector (shuffle mheights) (shuffle mweights))
                 (map #(hash-map :class :male :datum %)))
            (->> (map vector (shuffle fheights) (shuffle fweights))
                 (map #(hash-map :class :female :datum %))))))

(def training-data
  (gen-people 100))

(def real-data
  (gen-people 1000))

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
  [real-data train-data k]
  (let [result (map #(classify % train-data k) real-data)
        success (count (filter #(= (:class %) (:result %)) result))
        error (- (count real-data) success)]
    {:data result
     :summary {:success success :error error}} ))

