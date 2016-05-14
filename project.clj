(defproject bv "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [funcool/cats "1.2.1"]
                 [amazonica "0.3.44"]
                 [liberator "0.13"]
                 [compojure "1.3.4"]
                 [metosin/compojure-api "1.0.0-RC1"]
                 [ring/ring-core "1.2.1"]]
  :plugins [[lein-ring "0.8.11"]]
  :ring {:handler bv.api/handler}
  :aot [bv.core]
  :main ^:skip-aot bv.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
