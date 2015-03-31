(ns rksm.cloxp-cljs.compilation
  (:require [clojurescript-build.core :as cljsb]
            [rksm.system-files :as sf]
            [rksm.system-files.fs-util :as fs-util]
            [rksm.system-files.cljx :as cljx-files]
            [rksm.cloxp-cljs.filemapping :refer [cp-dirs-with-cljs]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [cljs.closure :as cljsc]
            [cljs.env :as env]
            [leiningen.core.project :as lein-proj]
            [leiningen.cljsbuild.config :as cljs-config]))

(def ^{:dynamic true} *optimizations* :none)

(defn default-build-options
  [project-dir]
  (let [target-dir (.getCanonicalPath (io/file (str project-dir "/cloxp-cljs-build/")))
        target-file (str target-dir java.io.File/separator "cloxp-cljs.js")
        out-dir (str target-dir java.io.File/separator "out")
        source-map-file (str target-file ".map")]
    {:output-to target-file
     :output-dir out-dir
     :optimizations :none
     :cache-analysis true
     ;; :source-map true
     :warnings true}))

(defonce builds (atom {}))

(defn- cljs-source-paths
  [project-dir]
  (->> (cp-dirs-with-cljs project-dir)
    keys
    fs-util/remove-parent-paths
    (map #(.getCanonicalPath %))))

(defn- compile-cljx-in-project
  [ns-name file project-dir]
  (if (cljx-files/cljx-file? file)
    (let [out-file (io/file (str project-dir "/target/classes/"
                                 (sf/ns-name->rel-path ns-name ".cljs")))]
      (-> out-file .getParentFile .mkdirs)
      (spit out-file (.getCljs (sf/file file))))))

(defn compile-cljs-in-project
  [changed-ns file project-dir & [compiler-env]]
  (if changed-ns
    (compile-cljx-in-project changed-ns file project-dir))
  (let [build-opts (default-build-options project-dir)
        cljs-source-paths (cljs-source-paths project-dir)
        compiler-env (or compiler-env (env/default-compiler-env))]
    (env/with-compiler-env compiler-env
      (let [build (if-let [build (get @builds project-dir)]
                    (cljsb/build-source-paths* build)
                    (cljsb/build-source-paths cljs-source-paths build-opts))]
        (swap! builds assoc project-dir build)
        build))))


; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; DEPRECATED (?)

(defn compile-cljs
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

(defn compile-all-cljs
  [dir compiler-env]
  (let [target-dir (.getCanonicalPath (io/file "./cloxp-cljs-build/"))]
    (env/with-compiler-env (:compiler-env compiler-env)
      (cljsc/build dir {:optimizations *optimizations*
                        :output-dir target-dir
                        :cache-analysis true}))))