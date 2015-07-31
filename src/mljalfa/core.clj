(ns mljalfa.core
  (:use [incanter.charts :only [xy-plot add-points]]
        [incanter.core :only [view]])
  (:require
    [clatrix.core :as cl]
    [clojure.core.matrix :refer :all]
    [clojure.core.matrix.operators :as M]))

(defn ^long sum-sieve
  "Return the sum of all primes at most lim"
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        hlim (if (even? llim) (inc llim) (+ 2 llim))
        primes (boolean-array (+ lim 1) true)
        ;; this is the initial loop for i <= (isqrt lim)
        start (loop [i (int 3) res (int 2)]
                (if (> i llim)
                  res
                  (if (aget primes i)
                    (do (loop [j (int (* i i))]
                          (when (<= j lim)
                            (aset primes j false)
                            (recur (+ j i i))))
                        (recur (+ i 2) (+ res i)))
                    (recur (+ i 2) res))))]
    (loop [i (int hlim) res (int start)]
      (if (> i lim)
        res
        (if (aget primes i)
          (recur (+ i 2) (+ i res))
          (recur (+ i 2) res))))))


(defn ^long sum-primes
  [^long lim]
  (let [odd-prime? (fn [^long n]
                     (let [lim (int (Math/sqrt n))]
                       (loop [i (int 3)]
                         (cond (> i lim) true
                               (== 0 (rem n i)) false
                               :else (recur (+ i 2))))))]
    (+ 2 (transduce
           (filter odd-prime?)
           + (range 3 lim 2)))))

(def mat1 (cl/matrix [[1 2 3] [2 3 4]]))
(def mat2 (cl/matrix [[4 3 2] [5 4 3]]))

(defn square-matrix
  [n e]
  (let [repeater #(repeat n %)]
    (cl/matrix (-> e repeater repeater))))

(defn plot-points
  "Plots sample points of a solution s"
  [s]
  (let [X (concat (:hidden s) (:observed s))
        Y (concat (:hidden-values s) (:observed-values s))]
    (view
      (add-points
        (xy-plot X Y) (:observed s) (:observed-values s)))))

(defn lmatrix [n]
  (compute-matrix :clatrix [n (+ n 2)]
                  (fn [i j] ({0 -1, 1 2, 2 -1} (- j i) 0))))

(defn problem
  "Return a map of the problem setup for a
  given matrix size, number of observed values
  and regularization parameter"
  [n n-observed lambda]
  (let [i (shuffle (range n))]
    {:L               (M/* (lmatrix n) lambda)
     :observed        (take n-observed i)
     :hidden          (drop n-observed i)
     :observed-values (matrix :clatrix
                              (repeatedly n-observed rand))}))

(defn solve
  "Return a map containing the approximated value
   y of each hidden point x"
  [{:keys [L observed hidden observed-values] :as problem}]
  (let [nc (column-count L)
        nr (row-count L)
        L1 (cl/get L (range nr) hidden)
        L2 (cl/get L (range nr) observed)
        l11 (M/* (transpose L1) L1)
        l12 (M/* (transpose L1) L2)]
    (assoc problem :hidden-values
                   (M/* -1 (inverse l11) l12 observed-values))))

(defn mat=
  "Equality check for matrix"
  [A B]
  (and (== (count A) (count B))
       (reduce #(and %1 %2) (map = A B))))

(defn mat+
  "Matrix addition for two or more matrix"
  ([A B] (mapv #(mapv + %1 %2) A B))
  ([A B & more]
    (let [mat (concat [A B] more)]
      (reduce mat+ mat))))




















