(ns mljalfa.logger
  (:require
    [org.httpkit.client :as http]
    [clojure.data.json :as json]
    [clojure.edn :as edn]))

(def host "http://localhost:3000/")

(defn get-json
  "Get something from julia"
  [url]
  (-> (str host url)
      (http/get {:keepalive -1})
      deref :body
      (json/read-json true)
      time))






