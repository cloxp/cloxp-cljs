(ns rksm.cloxp-cljs.filemapping
  (:require [rksm.system-files :as sf]
            [rksm.system-files.fs-util :as fs]))

(defn cp-dirs-with-cljs
  "Returns a map of dir/files, dir being the classpath, files being cljs files
  in it"
  [& [parent-dir]]
  (let [map (->> (sf/classpath-dirs)
              (mapcat (fn [d ] {d (fs/walk-dirs d #".*\.cljs$")}))
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

(defn cljs-files-in-cp-dirs
  []
  (->> (sf/classpath-dirs)
    (mapcat #(fs/walk-dirs % #".*\.cljs$"))))

(defn find-file-for-ns-on-cp
  [ns-name]
  (or
   (let [path (sf/ns-name->rel-path ns-name ".cljs")]
     (some->> (cp-dirs-with-cljs)
       (mapcat (fn [[dir files]]
                 (filter 
                  #(= path (fs/path-relative-to dir (.getCanonicalPath %)))
                  files)))
       first
       .getCanonicalPath))
   (sf/jar-url-for-ns ns-name ".cljs")))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn find-cljs-namespaces-on-cp
  []
  (flatten
   (for [[cp-dir files] (cp-dirs-with-cljs)]
     (let [dir (.getCanonicalPath cp-dir)]
       (map #(sf/rel-path->ns-name
               (fs/path-relative-to dir (.getCanonicalPath %))) files)))))

(comment
 (cljs-files-in-cp-dirs)
 (find-file-for-ns-on-cp 'rksm.test)
 (find-file-for-ns-on-cp 'cljs.core.async.impl.dispatch)
 (.file (ClassLoader/getSystemResource "cljs/core/async.cljs"))
 
 (def jar-string (.toString (.toURI (ClassLoader/getSystemResource "cljs/core/async.cljs"))))
 (clojure.string/split #"!" jar-string)
 (java.util.jar.JarFile. )
 
 (slurp (clojure.java.io/file (.toString (.toURI (ClassLoader/getSystemResource "cljs/core/async.cljs")))))
 (slurp (clojure.java.io/reader (ClassLoader/getSystemResource "cljs/core/async.cljs")))
 
 
 )