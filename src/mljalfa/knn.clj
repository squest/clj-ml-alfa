(ns mljalfa.knn)

(defn sqr [x] (* x x))

(defn distance
  [p1 p2]
  (reduce + (map #(sqr (- %1 %2)) p1 p2)))



