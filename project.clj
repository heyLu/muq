(defproject muq "0.1-dev"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2173"]
                 [org.clojure/data.fressian "0.2.0"]
                 [com.datomic/datomic-free "0.9.4556"]]
  :plugins [[lein-cljsbuild "1.0.2"]]
  :cljsbuild {
    :builds [{:id "default"
              :source-paths ["src"]
              :compiler {:output-to "target/muq.js"}}
             {:id "optimized"
              :source-paths ["src"]
              :compiler {:output-to "target/muq.min.js"
                         :source-map "target/muq.min.js.map"
                         :externs ["externs.js"]
                         :optimizations :advanced}}]})
