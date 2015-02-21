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
  (let [path (sf/ns-name->rel-path ns-name ".cljs")]
    (->> (cp-dirs-with-cljs)
      (mapcat (fn [[dir files]]
                (filter 
                 #(= path (fs/path-relative-to dir (.getCanonicalPath %)))
                 files)))
      first)))

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
 )