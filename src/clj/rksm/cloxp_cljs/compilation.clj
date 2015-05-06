(ns rksm.cloxp-cljs.compilation
  (:require [clojurescript-build.core :as cljsb]
            [rksm.system-files :as sf]
            [rksm.system-files.fs-util :as fs-util]
            [rksm.system-files.cljx :as cljx]
            [rksm.cloxp-cljs.filemapping :refer [cp-dirs-with-cljs]]
            [rksm.cloxp-repl :as repl]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [cljs.closure :as cljsc]
            [cljs.env :as env]
            [leiningen.cljsbuild.config :as cljs-config]))

(def ^{:dynamic true} *optimizations* :none)

(defn default-build-options
  [project-dir]
  (let [target-dir (.getCanonicalPath (io/file project-dir "cloxp-cljs-build/")) 
        target-file (str target-dir java.io.File/separator "cloxp-cljs.js")
        out-dir (str target-dir java.io.File/separator "out")
        source-map-file (str target-file ".map")]
    {:output-to target-file
     :output-dir out-dir
     :optimizations :none
     :cache-analysis true
     :source-map source-map-file
     :warnings true}))

(defonce builds (atom {}))

(defn- cljs-source-paths
  [project-dir]
  (->> (cp-dirs-with-cljs project-dir)
    keys
    fs-util/remove-parent-paths
    (map #(.getCanonicalPath %))))

(defn compile-cljs-in-project
  [changed-ns file project-dir new-source old-source & [compiler-env]]
  (when (and changed-ns (cljx/cljx-file? file))
    (cljx/ns-compile-cljx->cljs changed-ns file project-dir)
    (repl/load-file new-source (str file) {:old-source old-source}))
  (let [build-opts (default-build-options project-dir)
        cljs-source-paths (cljs-source-paths project-dir)
        compiler-env (or compiler-env env/*compiler* (env/default-compiler-env))]
    (env/with-compiler-env compiler-env
      (let [build (if-let [build (get @builds project-dir)]
                    (cljsb/build-source-paths* build)
                    (cljsb/build-source-paths cljs-source-paths build-opts))]
        (swap! builds assoc project-dir build)
        build))))

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
         (default-build-options "/Users/robert/clojure/cloxp-cljs-repl")
         )

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; DEPRECATED (?)

#_(defn compile-cljs
  [ns-name file]
  (let [target-dir (.getCanonicalPath (io/file "./cloxp-cljs-build/"))
        target-file (str target-dir java.io.File/separator
                         (-> (io/file file)
                           .getName
                           (s/replace #"\.clj(.)?$" ".js")))
        source-map-file (str target-file ".map")]
    (cljsc/build file {:optimizations *optimizations*
                       :output-to target-file
                       :output-dir target-dir
                       :cache-analysis true
                    ;   :source-map source-map-file
                       })))

#_(defn compile-all-cljs
  [dir compiler-env]
  (let [target-dir (.getCanonicalPath (io/file "./cloxp-cljs-build/"))]
    (env/with-compiler-env (:compiler-env compiler-env)
      (cljsc/build dir {:optimizations *optimizations*
                        :output-dir target-dir
                        :cache-analysis true}))))