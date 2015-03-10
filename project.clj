(defproject org.rksm/cloxp-cljs "0.1.1-SNAPSHOT"
  :description "cloxp-cljs"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-3031"]
                 [org.rksm/system-files "0.1.3-SNAPSHOT"]
                 [org.rksm/cloxp-source-reader "0.1.0-SNAPSHOT"]
                 [clojurescript-build/clojurescript-build "0.1.5"]]
  :source-paths ["src/clj" "src/cljs"]
  :test-paths ["src/test"]
  :deploy-repositories [["releases" :clojars]]
  :hiera {:path "target/ns-hierarchy.png"
          :show-external true
          :ignore-ns #{}})
