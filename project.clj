(defproject jack-frost "0.1.0-SNAPSHOT"
  :description "Fast serialization for CLJS"
  :url "http://github.com/halgari/deep-freeze"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :plugins [[lein-cljsbuild "0.3.2"]]
  :cljsbuild {:builds [{:source-paths ["src-cljs"]
                        :compiler {:output-to "target/main.js"  ; default: target/cljsbuild-main.js
                                   ;;:pretty-print true
                                   :optimizations :advanced}}]})

