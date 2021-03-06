(defproject mljalfa "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [net.mikera/core.matrix "0.36.1"]
                 [clatrix "0.5.0"]
                 [expectations "2.1.2"]
                 [incanter "1.9.0"]
                 [http-kit "2.1.19"]
                 [couchbase-clj "0.2.0"]
                 [selmer "0.9.1"]
                 [cheshire "5.5.0"]
                 [clj-time "0.10.0"]
                 [clojurewerkz/statistiker "0.1.0-SNAPSHOT"]
                 [io.forward/clojure-mail "1.0"]]
  :plugins [[lein-expectations "0.0.8"]
            [lein-gorilla "0.3.4"]])
