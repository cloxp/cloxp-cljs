(ns rksm.cloxp-cljs.compilation
  (:require [clojurescript-build.core :as cljsb]
            [rksm.system-files :as sf]
            [rksm.system-files.fs-util :as fs-util]
            [rksm.cloxp-cljs.filemapping :refer [cp-dirs-with-cljs find-file-for-ns-on-cp]]
            [rksm.cloxp-repl :as repl]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [cljs.closure :as cljsc]
            [cljs.env :as env]
            [cljs.build.api :as build]
            [cljs.compiler.api :as compiler]
            [leiningen.cljsbuild.config :as cljs-config]))

(def ^{:dynamic true} *optimizations* :none)

(defn default-build-options
  [project-dir]
  (let [target-dir (.getCanonicalPath (io/file project-dir "cloxp-cljs-build/"))
        target-file (str target-dir java.io.File/separator "cloxp-cljs.js")
        out-dir (str target-dir java.io.File/separator "out")
        source-map-file (str target-file ".map")]
    (build/add-implicit-options
     {:output-to target-file
      :output-dir out-dir
      :optimizations :none
      :cache-analysis true
      :source-map source-map-file
      :warnings true})))

(defonce builds (atom {}))

(defn- cljs-source-paths
  [project-dir]
  (->> (cp-dirs-with-cljs project-dir)
    keys
    fs-util/remove-parent-paths
    (map #(.getCanonicalPath %))))

(defn- ensure-ns-is-recompiled
  [ns-sym file source build-opts comp-env]
  (env/with-compiler-env comp-env
    (let [out-dir (:output-dir build-opts)
          target-file (build/src-file->target-file file build-opts)
          ext (re-find #"\.clj.?$" (str file))
          target-clj-file (sf/file (s/replace target-file #"\.js$" ext))]
      (.delete target-file)
      (spit target-clj-file source)
      (compiler/compile-file file target-file build-opts))))

(defn compile-cljs-in-project
  [changed-ns file project-dir & [new-source old-source compiler-env]]
  (if-let [file (or file (find-file-for-ns-on-cp changed-ns))]
    (let [new-source (or new-source (slurp file))
          old-source new-source
          build-opts (default-build-options project-dir)
          compiler-env (or compiler-env env/*compiler* (env/default-compiler-env))]
      (build/build project-dir build-opts compiler-env)
      (ensure-ns-is-recompiled changed-ns file new-source build-opts compiler-env))
    (throw (Exception. (str "Cannot retrieve cljs file for " changed-ns)))))

(comment 
 (cljs.closure/src-file->target-file
   "/Users/robert/Lively/LivelyKernel/cloxp-cljs-scratch/src/cljs/rksm/cljs_workspace_test.cljs"
   (default-build-options "/Users/robert/Lively/LivelyKernel/cloxp-cljs-scratch")))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; cljs build related

(defonce cljsbuild-reload-lib-orig cljsb/reload-lib)

(defn patch-cljs-build-reload
  []
  (alter-var-root
   #'cljsb/reload-lib
   (fn [_] (fn [resource]))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn clean
  [project-dir & [opts]]
  (let [opts (or opts (default-build-options project-dir))]
    (cljsb/clean-build opts)))

(comment (clean "/Users/robert/clojure/cloxp-cljs-repl")
         (clean "/Users/robert/clojure/cloxp-com")
         (default-build-options "/Users/robert/clojure/cloxp-cljs-repl"))