(ns rksm.cloxp-cljs.filemapping
  (:require [rksm.system-files :as sf]
            [rksm.system-files.fs-util :as fs-util]
            [rksm.system-files.jar-util :as jar]
            [rksm.system-files.cljx :as cljx]
            [clojure.java.io :as io])
  (:import (rksm.system-files.cljx.File)))

(defn cp-dirs-with-cljs
  "Returns a map of dir/files, dir being the classpath, files being cljs files
  in it"
  [& [parent-dir]]
  (let [map (->> (sf/classpath-dirs)
              (remove #(re-find #"target/classes/?$" (str %)))
              (mapcat (fn [d ] {d (fs-util/walk-dirs d #".*\.clj(s|x)$")}))
              (filter (comp not-empty second))
              (map (partial apply hash-map))
              (apply merge))]
    (if parent-dir
      (let [project-path (.getCanonicalPath (clojure.java.io/file parent-dir))]
        (->> map
          (filter (fn [[dir _]] (-> dir
                                  .getCanonicalPath
                                  (.startsWith project-path))))
          (into {})))
      map)))

(defn- find-file-in-cp-dirs
  [filename]
  (let [path (re-pattern (str "^" (sf/ns-name->rel-path 'foo ".clj(s|x)$")))]
    (some->> (cp-dirs-with-cljs)
      (mapcat (fn [[dir files]]
                (filter 
                 #(= filename (fs-util/path-relative-to dir (.getCanonicalPath %)))
                 files)))
      first .getCanonicalPath)))

(defn find-file-for-ns-on-cp
  [ns-name & [file]]
  (when-let [file (if file
                    (sf/file file)
                    (sf/file
                     (or
                      (find-file-in-cp-dirs (sf/ns-name->rel-path ns-name ".cljx"))
                      (find-file-in-cp-dirs (sf/ns-name->rel-path ns-name ".cljs"))
                      (jar/jar-url-for-ns ns-name ".cljx")
                      (jar/jar-url-for-ns ns-name ".cljs")
                      (sf/file-for-ns ns-name nil #"\.clj(s|x)$"))))]
    (if (instance? rksm.system-files.cljx.File file)
      (doto file (.changeMode :cljs)))
    file))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

; (defn find-cljs-namespaces-on-cp
;   []
;   (flatten
;   (for [[cp-dir files] (cp-dirs-with-cljs)]
;      (let [dir (.getCanonicalPath cp-dir)]
;       (map #(sf/rel-path->ns-name
;               (fs/path-relative-to dir (.getCanonicalPath %))) files)))))

(defn find-cljs-namespaces-on-cp
  []
  (sf/find-namespaces-on-cp #"\.cljs$"))

(comment
 (find-file-for-ns-on-cp 'rksm.test)
 (find-file-for-ns-on-cp 'cljs.core.async.impl.dispatch)
 (find-file-for-ns-on-cp 'clojure.zip)
 (rksm.cloxp-cljs.ns.internals/analyze-cljs-ns! 'clojure.zip)

 (find-cljs-namespaces-on-cp)
 (.file (ClassLoader/getSystemResource "cljs/core/async.cljs"))
 
 (def jar-string (.toString (.toURI (ClassLoader/getSystemResource "cljs/core/async.cljs"))))
 (clojure.string/split #"!" jar-string)
 (java.util.jar.JarFile. )
 
 (slurp (clojure.java.io/file (.toString (.toURI (ClassLoader/getSystemResource "cljs/core/async.cljs")))))
 (slurp (clojure.java.io/reader (ClassLoader/getSystemResource "cljs/core/async.cljs")))
 
 
 )