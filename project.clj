(defproject nl.surf/demo-data "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[hiccup "1.0.5"]
                 [io.forward/yaml "1.0.9" :exclusions [org.flatland/ordered]]
                 [log4j/log4j "1.2.17"]
                 [markov-chain "0.1.0"]
                 [nl.zeekat/ring-openapi-validator "0.1.0"]
                 [org.clojure/clojure "1.10.1"]
                 [org.clojure/data.generators "0.1.2"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/tools.logging "0.5.0"]
                 [org.flatland/ordered "1.5.7"] ; compatible with JDK 11
                 [org.slf4j/slf4j-log4j12 "1.7.30"]
                 [ring/ring-jetty-adapter "1.8.0"]
                 [ring/ring-json "0.5.0"]
                 [ring/ring-devel "1.8.0"]]
  :repl-options {:init-ns nl.surf.demo-data.world}
  :resource-paths ["resources" "generated"])
