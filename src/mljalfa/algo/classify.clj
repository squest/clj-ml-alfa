(ns mljalfa.algo.classify)

(def sqrt #(Math/sqrt %))
(def sqr #(* % %))

(defn edistance-squared
  "Given n-dimensional points p1 & p2, return the euclidean distance squared"
  [p1 p2]
  (reduce + (map #(sqr (- %1 %2)) p1 p2)))

(defn euclidean-distance
  "Given n-dimensional point p1 & p2, returns the euclidean distance"
  [p1 p2]
  (sqrt (edistance-squared p1 p2)))

(defn centroid
  "Given a list of vectors of the same dimension, returns the centroid of those data"
  [xs]
  )
