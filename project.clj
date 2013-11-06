(defproject cljs-pong "0.1.0-SNAPSHOT"
  :description "Quick Pong Implementation"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1934"
                   :exclusions [org.apache.ant/ant]]]
  :plugins [[lein-cljsbuild "0.3.4"]]
  :cljsbuild {:builds
               [{:source-paths ["cljs-src/pong"],
                  :builds nil,
                  :compiler
                  {:pretty-print true,
                    :output-to "resources/public/js/main.js",
                    :optimizations :whitespace}}],
               :repl-listen-port 9000})
