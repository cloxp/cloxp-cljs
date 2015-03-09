(ns rksm.cloxp-cljs.compilation
  (:require [clojurescript-build.core :as cljsb]
            [rksm.system-files.fs-util :as fs-util]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [cljs.closure :as cljsc]
            [cljs.env :as env]
            [rksm.cloxp-cljs.filemapping :refer [cp-dirs-with-cljs]]))

(def ^{:dynamic true} *optimizations* :none)

(defonce builds (atom {}))

(defn- cljs-source-paths
  [project-dir]
  (fs-util/remove-parent-paths
  (keys (cp-dirs-with-cljs project-dir))))

(defn compile-cljs-in-project
  [project-dir & [compiler-env]]
  (let [target-dir (.getCanonicalPath (io/file (str project-dir "/cloxp-cljs-build/")))
        target-file (str target-dir java.io.File/separator "cloxp-cljs.js")
        out-dir (str target-dir java.io.File/separator "out")
        source-map-file (str target-file ".map")
        build-opts {:output-to target-file
                    :output-dir out-dir
                    :optimizations :none
                    :cache-analysis true
                    ;; :source-map true
                    :warnings true}
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