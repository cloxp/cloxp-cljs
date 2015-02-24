(ns rksm.cloxp-cljs.ns.internals
  (:require [cljs.analyzer :as ana]
            [cljs.env :as env]
            [rksm.cloxp-cljs.filemapping :as fm]
            [rksm.system-files :as sf]
            [clojure.data.json :as json]
            [clojure.string :as s]
            [clojure.tools.reader :as tr]
            [cljs.closure :as cljsc])
  (:import (java.io LineNumberReader PushbackReader File)))

(declare analyzed-data-of-def)

(defonce cljs-env (atom {}))


(defn ensure-default-cljs-env
  []
  (if-let [e (:default @cljs-env)]
    e
    (swap! cljs-env assoc
           :default
           {:analyzer-env (atom (merge (ana/empty-env) {:ns 'cljs.user}))
            :compiler-env (env/default-compiler-env)})))


; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn- read-next-obj
  "follows the reader while it core/reads an object and returns the string in
  range of what was read"
  [rdr]
  (let [text (StringBuilder.)
        pbr (proxy [PushbackReader] [rdr]
                   (read []
                         (let [i (proxy-super read)]
                           (if (> i -1) (.append text (char i)))
                           i)))]
    (if (= :unknown *read-eval*)
      (throw (IllegalStateException. "Unable to read source while *read-eval* is :unknown."))
      (tr/read (PushbackReader. pbr) false nil))
    (str text)))

(defn- read-entity-source
  "goes forward in line numbering reader until line of entity is reached and
  reads that as an object"
  [{lrdr :lrdr, sources :sources, :as record} meta-entity]
  (or (if-let [line (:line meta-entity)]
        (do
          (dotimes [_ (dec (- line (.getLineNumber lrdr)))] (.readLine lrdr))
          (let [updated (assoc meta-entity :source (read-next-obj lrdr))]
            (update-in record [:sources] conj updated))))
      record))

(defn merge-source
  "reads objects at specified columns / lines"
  [rdr meta-infos]
  (let [source-data {:lrdr (LineNumberReader. rdr), :sources []}]
    (if-let [result (reduce read-entity-source source-data meta-infos)]
      (:sources result)
      meta-infos)))

(defn source-for-symbol
  [sym & [file]]
  (let [file-data (some-> (analyzed-data-of-def sym file)
                    (select-keys [:column :line :file]))
        jar-match (re-find #"^file:(.*\.jar)!.*" (:file file-data))
        rdr (if jar-match
              (sf/jar-reader-for-ns
               (second jar-match) (namespace sym) ".cljs")
              (clojure.java.io/reader (:file file-data)))]
    (some->> [file-data]
      (merge-source rdr)
      first :source)))

(defn source-for-ns
  [sym & [file]]
  (slurp
   (clojure.java.io/file
    (or file
        (fm/find-file-for-ns-on-cp sym)))))


; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn intern-info
  [analyzed-def]
  (let [qname (:name analyzed-def)
        name (name qname)
        ns (namespace qname)
        base (select-keys analyzed-def [:file :column :line])]
    (assoc base :ns ns :name name)))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn symbol-info-for-macro
  [ns-name name]
  (some-> (ns-interns ns-name) (get name) meta))

(defn symbol-info-for-sym
  "find what we know about symbol in a given namespace. This is *not* asking
  for the meta data of a var, rather looking up what symbol is bound to what
  thing in a given namespaces"
  [ns-name sym & [file]]
  (if-let [ns-data (some-> (ensure-default-cljs-env)
                     :compiler-env
                     deref
                     :cljs.analyzer/namespaces)]
    (if-let [source-ns-data (get ns-data ns-name)]
      (let [sym-name (symbol (name sym))]
        (if-let [sym-ns (some-> sym namespace symbol)]

          ; for qualified symbols like s/join or clojure.string/join
          (if-let [macro-ns (some-> source-ns-data :require-macros (get sym-ns))] ; macro?
            (symbol-info-for-macro macro-ns sym-name)

            ; required?
            (let [full-ns-name (or (some-> source-ns-data :requires (get sym-ns)) sym-ns)
                  qname (symbol (str (or full-ns-name sym-ns)) (str sym-name))]
              (analyzed-data-of-def qname file)))

          ; unqualified symbols
          (if-let [def-data (some-> ns-data ; def in current ns?
                              (get ns-name) :defs (get sym-name))]
            (assoc def-data :ns ns-name)

            ; macro?
            (if-let [macro-ns (some-> source-ns-data :use-macros (get sym-name))]
              (symbol-info-for-macro macro-ns sym-name)

              ; referred?
              (if-let [target-ns-name (some-> source-ns-data :uses (get sym-name))]
                (symbol-info-for-sym target-ns-name sym-name)))))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn namespace-info
  [ns-name & [file]]
  (let [file (or file (fm/find-file-for-ns-on-cp ns-name))]
    (if-let [data (env/with-compiler-env (:compiler-env (ensure-default-cljs-env))
                                         (do
                                           (if file (ana/analyze-file (clojure.java.io/file file)))
                                           (some-> env/*compiler*
                                             deref
                                             :cljs.analyzer/namespaces
                                             (get ns-name))))]
      (-> data
        (select-keys [:name :doc :excludes :use :require :uses :requires :imports])
        (assoc :file (if file (str file)))
        (assoc :interns (map intern-info (vals (:defs data))))))))

(defn stringify [obj]
  (cond
    (var? obj) (:name (meta obj))
    (or (string? obj) (symbol? obj) (keyword? obj)) (name obj)
    (or (seq? obj)) (vec obj)
    (or (map? obj)) obj
    ; it seems that the value-fn in json/write is not called for every value
    ; individually, only for map / collection like things... so to filter out
    ; objects that couldn't be stringified to start with we have this map stmt in
    ; here...
    (coll? obj) (map #(if (some boolean ((juxt map? coll?) %)) % (stringify %)) obj)
    :else (str obj)))

(defn jsonify
  [obj]
  (json/write-str obj
                  :key-fn #(if (keyword? %) (name %) (str %))
                  :value-fn (fn [_ x] (stringify x))))

(defn symbol-info-for-sym->json [ns-name sym & [file]]
  (jsonify (symbol-info-for-sym ns-name sym file)))

(defn namespace-info->json [ns & [file-path]]
  (jsonify (namespace-info ns file-path)))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn find-cljs-namespaces-in-env
  [& [env-name]]
  (or (if-let [env (or (and env-name (get @cljs-env env-name))
                       (ensure-default-cljs-env))]
        (some-> (:compiler-env env)
          deref :cljs.analyzer/namespaces keys))
      []))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(def ^{:dynamic true} *optimizations* :none)

(defn compile-cljs
  [ns-name file]
  (let [target-dir (.getCanonicalPath (clojure.java.io/file "./cloxp-cljs-build/"))
        target-file (str target-dir java.io.File/separator
                         (-> (clojure.java.io/file file)
                           .getName
                           (s/replace #"\.clj(.)?$" ".js")))
        source-map-file (str target-file ".map")]
    (sf/add-classpath target-dir)
    (cljsc/build file {:optimizations *optimizations*
                       :output-to target-file
                       :output-dir target-dir
                       :cache-analysis true
                       :source-map source-map-file})))

(defn compile-all-cljs
  [dir]
  (let [target-dir "./cloxp-cljs-build/"]
    (sf/add-classpath target-dir)
    (env/with-compiler-env (:compiler-env (ensure-default-cljs-env))
      (cljsc/build dir {:optimizations *optimizations*
                        :output-dir target-dir
                        :cache-analysis true}))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn update-source-file!
  [sym new-source old-src file]
  (let [info (analyzed-data-of-def sym file)
        ns-sym (symbol (namespace sym))
        file (or file (fm/find-file-for-ns-on-cp ns-sym))
        old-file-src (slurp file)
        new-file-src (sf/updated-source sym info new-source old-src old-file-src)]
    (spit file new-file-src)))

(defn eval-and-update-meta!
  "NOT YET IMPLEMENTED"
  [sym new-source]
  )

(defn record-change!
  "NOT YET IMPLEMENTED"
  [sym new-source old-src]
  {:sym sym
   :changes []})

(defn change-def!
  [sym new-source & [write-to-file file]]
  (let [old-src (source-for-symbol sym file)]
    (if (and old-src write-to-file)
      (update-source-file! sym new-source old-src file))
    (eval-and-update-meta! sym new-source)
    ; (update-source-pos-of-defs-below! sym new-source old-src)
    (let [old-src (source-for-symbol sym file)
          change (record-change! sym new-source old-src)]
      (dissoc change :source :prev-source))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn change-ns-in-runtime!
  "NOT YET IMPLEMENTED"
  [ns-name new-source old-src file]
  {:added []
   :removed []
   :changed []})

(defn record-change-ns!
  "NOT YET IMPLEMENTED"
  [ns-name new-source old-src diff]
  {:sym ns-name
   :changes []})

(defn change-ns!
  [ns-name new-source & [write-to-file file]]
  (if-let [old-src (source-for-ns ns-name file)]
    (do
      (if write-to-file
        (when-let [file (or file (fm/find-file-for-ns-on-cp ns-name))]
          (spit file new-source)
          (compile-cljs ns-name file)))
      (let [diff (change-ns-in-runtime! ns-name new-source old-src file)
            change (record-change-ns! ns-name new-source old-src diff)]
        change))
    (throw (Exception. (str "Cannot retrieve current source for " ns-name)))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment

 (reset! cljs-env {})
 (namespace-info 'rksm.test)

 (def file (first (rksm.cloxp-cljs.filemapping/cljs-files-in-cp-dirs)))
 (ana/forms-seq file)
 (rksm.cloxp-cljs.filemapping/find-cljs-namespaces-on-cp)

 (env/with-compiler-env (:compiler-env (ensure-default-cljs-env))
   (ana/analyze-file file))
 (env/with-compiler-env (:compiler-env (ensure-default-cljs-env))
   (get-in @env/*compiler* [::namespaces])
;   (ana/forms-seq file)
   )

 (source-for-symbol 'rksm.test/foo)


 (some-> (ensure-default-cljs-env)
   :compiler-env
   deref
  :cljs.analyzer/namespaces
  (get 'rksm.test)
  :defs
   (get 'foo)
   (select-keys [:column :line :file])
   )


 (namespace-info 'rksm.test)

 (rksm.system-navigator.ns.internals/namespace-info *ns*)
 (ensure-default-cljs-env)

 (:cljs.analyzer/analyzed-cljs :cljs.analyzer/namespaces :options :js-dependency-index)

 (env/with-compiler-env (:compiler-env (ensure-default-cljs-env))
   @ana/namespaces)

 (java.util.Date. (.lastModified file))
 (-> (clojure.java.io/file "/Users/robert/clojure/cloxp-cljs/out") .exists)
 (-> (clojure.java.io/file "/Users/robert/clojure/cloxp-repl/out") .exists)

 (System/setProperty "user.dir" "/Users/robert/clojure/cloxp-cljs")


 )