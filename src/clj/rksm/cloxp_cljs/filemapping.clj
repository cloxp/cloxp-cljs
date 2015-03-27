(ns rksm.cloxp-cljs.filemapping
  (:require [rksm.system-files :as sf]
            [rksm.system-files.fs-util :as fs]
            [clojure.java.io :as io]))

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

(defn- find-file-in-cp-dirs
  [filename]
  (let [path (sf/ns-name->rel-path ns-name ".cljs")]
    (some->> (cp-dirs-with-cljs)
      (mapcat (fn [[dir files]]
                (filter 
                 #(= filename (fs/path-relative-to dir (.getCanonicalPath %)))
                 files)))
      first
      .getCanonicalPath)))

(defn find-file-for-ns-on-cp
  [ns-name]
  (if-let [file (or
                 (find-file-in-cp-dirs (sf/ns-name->rel-path ns-name ".cljs"))
                 (find-file-in-cp-dirs (sf/ns-name->rel-path ns-name ".cljx"))
                 (sf/jar-url-for-ns ns-name ".cljs")
                 (sf/file-for-ns ns-name nil #"\.clj(s|x)$"))]
    (let [file-name (if (string? file) file (.getCanonicalPath file))]
     (cond
       (and
        (re-find #"^file:" file-name)
        (not (re-find #"\.jar$" file-name)))
       (io/file (second (re-find #"^file:(.*)" file-name)))
       (re-find #"\.cljx$" file-name)
       (doto (rksm.system-files.cljx.File. file-name) (.changeMode :cljs))
       :default file-name))))

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
 (find-cljs-namespaces-on-cp)
 (.file (ClassLoader/getSystemResource "cljs/core/async.cljs"))
 
 (def jar-string (.toString (.toURI (ClassLoader/getSystemResource "cljs/core/async.cljs"))))
 (clojure.string/split #"!" jar-string)
 (java.util.jar.JarFile. )
 
 (slurp (clojure.java.io/file (.toString (.toURI (ClassLoader/getSystemResource "cljs/core/async.cljs")))))
 (slurp (clojure.java.io/reader (ClassLoader/getSystemResource "cljs/core/async.cljs")))
 
 
 )