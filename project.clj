(defproject nl.surf/demo-data "1.0.1"
  :description "SURFnet DemoData toolkit"
  :license {:name "GPLv3"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :url "https://github.com/SURFnet/demo-data"
  :dependencies [[io.forward/yaml "1.0.9" :exclusions [org.flatland/ordered]]
                 [markov-chain "0.1.0"]
                 [org.clojure/clojure "1.10.1"]
                 [org.clojure/data.generators "0.1.2"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.flatland/ordered "1.5.7"] ; compatible with JDK 11
                 [cheshire/cheshire "5.9.0"]]
  :repl-options {:init-ns nl.surf.demo-data.world}
  :resource-paths ["resources" "generated"]
  :profiles {:dev     {:resource-paths ["dev-resources"]}
             :uberjar {:aot :all}}
  :deploy-repositories [["releases"  {:url "https://clojars.org/repo"
                                      :sign-releases false
                                      :username :env/clojars_username
                                      :password :env/clojars_password}]]
  :main nl.surf.demo-data
  :uberjar-name "demo-data-standalone.jar")
