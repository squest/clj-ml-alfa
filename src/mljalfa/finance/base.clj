(ns mljalfa.finance.base)

(def arpu-znet 200000)
(def arpu-agora 200000)
(def arpu-augmented 400000)

(defn revenue-others
  [cur percent]
  (* cur percent))

(defn revenue-znet
  [market-map]
  (let [relevant-market (select-keys market-map [:sd :smp :sma :smav])]
    (->> (vals relevant-market)
         (map #(reduce * (vals %)))
         (reduce +)
         (* arpu-znet)
         long str reverse
         (partition-all 3)
         (map reverse)
         (map #(apply str %))
         reverse
         (reduce #(str % "." %2)))))

(defn revenue-agora
  [market-map]
  (let [relevant-market (select-keys market-map [:college :public])]
    (->> (vals relevant-market)
         (map #(reduce * (vals %)))
         (reduce +)
         (* arpu-agora)
         (long))))


(def market
  {:sd      27328000
   :smp     9255000
   :sma     3942000
   :smav    3319000
   :college 5000000
   :public  10000000})

(def market-growth
  {:sd      {:growth          1.01
             :prospect-start  0.01
             :prospect-growth 1.05
             :adopter-start   0.01
             :adopter-growth  1.1}
   :smp     {:growth          1.02
             :prospect-start  0.03
             :prospect-growth 1.07
             :adopter-start   0.01
             :adopter-growth  1.6}
   :sma     {:growth          1.03
             :prospect-start  0.07
             :prospect-growth 1.1
             :adopter-start   0.017
             :adopter-growth  2.1}
   :smav    {:growth          1.04
             :prospect-start  0.02
             :prospect-growth 1.05
             :adopter-start   0.02
             :adopter-growth  1.4}
   :college {:growth          1.01
             :prospect-start  0.01
             :prospect-growth 1.05
             :adopter-start   0.01
             :adopter-growth  1.1}
   :public  {:growth          1.01
             :prospect-start  0.01
             :prospect-growth 1.05
             :adopter-start   0.01
             :adopter-growth  1.1}})

(defn revenue-projections-znet
  "Create a list of revenues for a number of year"
  [base-market growth-map year]
  (let [mkbase (select-keys base-market [:sd :smp :sma :smav])
        mkgrowth (select-keys growth-map [:sd :smp :sma :smav])]
    (loop [i 0 yoy-growth (merge-with #(assoc %1 :cur %2) mkgrowth mkbase) res []]
      (if (> i year)
        res
        (let [next-map
              (->> (vals yoy-growth)
                   (map #(hash-map
                          :cur (* (:growth %) (:cur %))
                          :growth (:growth %)
                          :prospect-start (* (:prospect-growth %)
                                             (:prospect-start %))
                          :prospect-growth (:prospect-growth %)
                          :adopter-growth (let [tmp (:adopter-start %)]
                                            (cond (<= 0 tmp 0.05)
                                                  (:adopter-growth %)
                                                  (<= 0.05 tmp 0.15)
                                                  (inc (* 4/5 (- (:adopter-growth %) 1)))
                                                  (<= 0.15 tmp 0.25)
                                                  (inc (* 3/5 (dec (:adopter-growth %))))
                                                  :else
                                                  (inc (* 2/5 (dec (:adopter-growth %))))))
                          :adopter-start (* (:adopter-growth %)
                                            (:adopter-start %))))
                   (zipmap (keys yoy-growth)))]
          (recur (inc i)
                 next-map
                 (conj res (->> (vals next-map)
                                (map #(dissoc % :adopter-growth
                                              :prospect-growth))
                                (zipmap (keys next-map))
                                (revenue-znet)
                                (assoc {:year (+ 2015 i)} :revenue)))))))))


