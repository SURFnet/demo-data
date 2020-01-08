(defproject nl.surf/prop "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.generators "0.1.2"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.flatland/ordered "1.5.7"] ; compatible with JDK 11
                 [ring/ring-jetty-adapter "1.7.1"]
                 [hiccup "1.0.5"]
                 [io.forward/yaml "1.0.9" :exclusions [org.flatland/ordered]]
                 [markov-chain "0.1.0"]]
  :repl-options {:init-ns nl.surf.prop}
  :resource-paths ["resources" "generated"])
