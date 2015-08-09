(defproject org.rksm/cloxp-cljs "0.1.10-SNAPSHOT"
  :description "cloxp-cljs"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "0.0-3308"]
                 [org.rksm/system-files "0.1.7-SNAPSHOT"]
                 [org.rksm/cloxp-source-reader "0.1.9-SNAPSHOT"]
                 [org.rksm/cloxp-repl "0.1.8-SNAPSHOT"]
                 [clojurescript-build/clojurescript-build "0.1.7"
                  :exclusions [org.clojure/clojurescript
                               org.clojure/core.async]]
                 [org.rksm/cloxp-projects "0.1.10-SNAPSHOT"]
                 [lein-cljsbuild "1.0.6"]]
  :profiles {:dev {:dependencies [[fs/fs "1.1.2"]]}}
  :source-paths ["src/clj" "src/cljs"]
  :test-paths ["src/test"]
  :clean-targets ["cloxp-cljs-build" :target-path]
  :deploy-repositories [["releases" :clojars]]
  :hiera {:path "target/ns-hierarchy.png"
          :show-external true
          :ignore-ns #{}})
