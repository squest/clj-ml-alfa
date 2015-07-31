(ns mljalfa.macros
  (:require [expectations :refer :all]))

(def testing-no (atom 1))

(defmacro def-test
  [what? expected real]
  (let [tmp (gensym)]
    `(let [~tmp (time ~real)]
       (do (println (str "Test no " @testing-no))
           (swap! testing-no inc)
           (println ~what?)
           (println (str "Expected : " ~expected))
           (println (str "Result : " ~tmp))
           (println)
           (expect ~expected ~tmp)))))

(defmacro deflet-test
  [what? binding expected real]
  (let [tmp (gensym)]
    `(let ~binding
       (let [~tmp (time ~real)]
         (do (println (str "Test no " @testing-no))
             (swap! testing-no inc)
             (println ~what?)
             (println (str "Expected : " ~expected))
             (println (str "Result : " ~tmp))
             (println)
             (expect ~expected ~tmp))))))

(reset! testing-no 1)
