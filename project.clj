(defproject org.rksm/cloxp-cljs "0.1.5"
  :description "cloxp-cljs"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-3126"]
                 [org.rksm/system-files "0.1.5"]
                 [org.rksm/cloxp-source-reader "0.1.4"]
                 [org.rksm/cloxp-repl "0.1.4"]
                 [clojurescript-build/clojurescript-build "0.1.5"]
                 [lein-cljsbuild "1.0.5"]
                 [leiningen/leiningen "2.5.1"]]
  :source-paths ["src/clj" "src/cljs"]
  :test-paths ["src/test"]
  :deploy-repositories [["releases" :clojars]]
  :hiera {:path "target/ns-hierarchy.png"
          :show-external true
          :ignore-ns #{}})
