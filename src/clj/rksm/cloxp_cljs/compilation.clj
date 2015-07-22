(ns rksm.cloxp-cljs.compilation
  (:require [clojurescript-build.core :as cljsb]
            [rksm.system-files :as sf]
            [rksm.system-files.fs-util :as fs-util]
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

#_(defn compile-cljs-in-project
  [changed-ns file project-dir new-source old-source & [compiler-env]]

  (repl/load-file new-source (str file) {:old-source old-source})

  (let [build-opts (default-build-options project-dir)
        cljs-source-paths (cljs-source-paths project-dir)
        compiler-env (or compiler-env env/*compiler* (env/default-compiler-env))]
    (env/with-compiler-env compiler-env
    ;   (cljsb/touch-or-create-file file 0)
      (let [build (if-let [build (some-> @builds
                                   (get project-dir)
                                   (assoc-in [:build-options :force] true)
                                   (assoc :compiler-env compiler-env)
                                   (assoc :additional-changed-ns [changed-ns]))]
                    (cljsb/build-source-paths* build)
                    (cljsb/build-source-paths cljs-source-paths build-opts compiler-env))]
        (swap! builds assoc project-dir build)
        build))))

(defn compile-cljs-in-project
  [changed-ns file project-dir new-source old-source & [compiler-env]]
  
  ; (repl/load-file new-source (str file) {:old-source old-source})
  
  (let [build-opts (default-build-options project-dir)
        compiler-env (or compiler-env env/*compiler* (env/default-compiler-env))]
    (cljs.closure/build project-dir build-opts compiler-env)
    (env/with-compiler-env compiler-env
      (cljs.closure/build
       (rksm.system-files/file file)
       build-opts)
      #_(cljs.closure/compile-file
         (rksm.system-files/file file)
         build-opts))))

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