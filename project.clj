(defproject mkremins/tailspin "0.1.0-SNAPSHOT"
  :dependencies
  [[org.clojure/clojure "1.6.0"]
   [org.clojure/clojurescript "0.0-2322"]
   [org.clojure/core.async "0.1.338.0-5c5012-alpha"]
   [com.facebook/react "0.11.1"]
   [markdown-clj "0.9.47"]
   [mkremins/dep-graph "0.1.0"]
   [om "0.7.1"]]

  :plugins
  [[lein-cljsbuild "1.0.3"]]

  :cljsbuild
  {:builds [{:source-paths ["src"]
             :compiler {:preamble ["react/react.js"]
                        :output-to "target/tailspin.js"
                        :source-map "target/tailspin.js.map"
                        :optimizations :whitespace}}]})
