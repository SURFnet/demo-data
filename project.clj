(defproject nl.surf/demo-data "1.1.0"
  :description "SURFnet DemoData toolkit"
  :license {:name "GPLv3"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :url "https://github.com/SURFnet/demo-data"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.generators "1.0.0"]
                 [org.clojure/math.combinatorics "0.2.0"]
                 [clj-commons/clj-yaml "1.0.27"]
                 [cheshire/cheshire "5.12.0"]
                 [markov-chain "0.2.0"]]
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
