(defproject org.rksm/cloxp-cljs "0.1.1-SNAPSHOT"
  :description "cloxp-cljs"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-3031"]
                 [org.rksm/system-files "0.1.3-SNAPSHOT"]
                 [clojurescript-build/clojurescript-build "0.1.5"]]
  :source-paths ["src/clj" "src/cljs" "src/test"]
  :deploy-repositories [["releases" :clojars]])
