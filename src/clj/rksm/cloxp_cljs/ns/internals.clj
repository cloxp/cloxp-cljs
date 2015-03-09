(ns rksm.cloxp-cljs.ns.internals
  (:require [cljs.analyzer :as ana]
            [cljs.env :as env]
            [rksm.cloxp-cljs.filemapping :as fm]
            [rksm.cloxp-cljs.compilation :as comp]
            [rksm.system-files :as sf]
            [clojure.data.json :as json]
            [clojure.string :as s]
            [clojure.tools.reader :as tr]
            [cljs.closure :as cljsc]
            [clojure.java.io :as io])
  (:import (java.io LineNumberReader PushbackReader File)))

(declare ensure-ns-analyzed! analyzed-data-of-def analyze-cljs-ns!)

(defonce cljs-env (atom {}))

(defn ensure-default-cljs-env
  []
  (if-let [e (:default @cljs-env)]
    e
    (let [e (swap! cljs-env assoc
                   :default
                   {:analyzer-env (atom (merge (ana/empty-env) {:ns 'cljs.user}))
                    :compiler-env (env/default-compiler-env)})]
      (if-not (some-> e
                :compiler-env deref
                :cljs.analyzer/namespaces (get 'cljs.core))
        (ensure-ns-analyzed! 'cljs.core))
      e)))


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

  ; ensure we have analyzed data for ns of sym
  (let [ns-name (symbol (namespace sym))] 
    (if-not (get-in
             @(:compiler-env (ensure-default-cljs-env))
             [:cljs.analyzer/namespaces ns-name])
      (ensure-ns-analyzed! ns-name)))

  (if-let [file-data (some-> (analyzed-data-of-def sym file)
                       (select-keys [:column :line :file]))]
    (let [def-file (:file file-data)
           rdr (if (sf/jar-clojure-url-string? def-file)
                 (sf/jar-url->reader def-file)
                 (io/reader def-file))]
      (some->> [file-data]
        (merge-source rdr)
        first :source))))


(defn source-reader-for-ns
  [ns-name & [file]]
  (if-let [file (or file (fm/find-file-for-ns-on-cp ns-name))]
    (if (sf/jar-clojure-url-string? file)
      (sf/jar-url->reader file)
      (io/reader file))))

(defn source-for-ns
  [sym & [file]]
  (slurp (source-reader-for-ns sym file)))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defn analyzed-data-of-def
  [sym & [file]]
  (let [ns-name (symbol (namespace sym))
        name (symbol (name sym))]

    (if-not (get-in ; in case ns data isn't there yet...
                    @(:compiler-env (ensure-default-cljs-env))
                    [:cljs.analyzer/namespaces ns-name])
      (ensure-ns-analyzed! ns-name))

    (some-> (ensure-default-cljs-env)
      :compiler-env deref
      :cljs.analyzer/namespaces (get ns-name)
      :defs (get name)
      (assoc :ns ns-name))))

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
  (let [cenv (:compiler-env (ensure-default-cljs-env))
        lenv (assoc (ana/empty-env) :ns ns-name)
        sym-name (symbol (name sym))]
    (if-let [ns-data (some-> cenv deref :cljs.analyzer/namespaces)]
      (if-let [source-ns-data (get ns-data ns-name)]
        (let [sym-ns (or
                      (some-> sym namespace symbol)
                      (if (get-in source-ns-data [:defs sym-name]) ns-name)
                      (if (cljs.env/with-compiler-env cenv
                            (ana/core-name? lenv sym-name)) 'cljs.core))
              macro-ns (or
                        (some-> source-ns-data :require-macros (get sym-ns))
                        (some-> source-ns-data :use-macros (get sym-name)))
              full-sym-ns (or (some-> source-ns-data :requires (get sym-ns))
                              (some-> source-ns-data :uses (get sym-name))
                              sym-ns)
              qname (if full-sym-ns
                      (symbol (str (or full-sym-ns sym-ns)) (str sym-name))
                      sym-name)]
          (if macro-ns
            (symbol-info-for-macro macro-ns sym-name)
            (analyzed-data-of-def qname file)))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn namespace-info
  [ns-name & [file]]
  (let [file (or file (fm/find-file-for-ns-on-cp ns-name))
        cenv (:compiler-env (ensure-default-cljs-env))]
    (if-let [
             data (analyze-cljs-ns! ns-name)
            ;  data (env/with-compiler-env cenv
            ;         (if file
            ;           (ana/analyze-file
            ;           (if (sf/jar-clojure-url-string? file)
            ;              (java.net.URL. file)
            ;              (io/file file))))
            ;         (some-> env/*compiler* deref
            ;           :cljs.analyzer/namespaces
            ;           (get ns-name)))
             ]
      (-> data
        (select-keys [:name :doc :excludes :use :require :uses :requires :imports])
        (assoc :file (if file (str file)))
        (assoc :interns (reverse (map intern-info (vals (:defs data)))))))))

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

(defn analyze-cljs-ns!
  [ns-sym]
  (let [cenv-atom (:compiler-env (ensure-default-cljs-env))
        path (fm/find-file-for-ns-on-cp ns-sym)
        rel-path (sf/ns-name->rel-path ns-sym ".cljs")]
    (cljs.env/with-compiler-env cenv-atom
      (swap! cljs.env/*compiler*
             (fn [cenv]
               (update-in cenv [:cljs.analyzer/namespaces] #(dissoc % ns-sym))
               (update-in cenv [:cljs.analyzer/analyzed-cljs] #(dissoc % path))))
      (cljs.analyzer/analyze-file rel-path {:cache-analysis false})
      (-> cljs.env/*compiler* deref
        :cljs.analyzer/namespaces
        (get ns-sym)))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(def ^:dynamic *compile?* true)

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
  [sym new-source]
  (let [ns-name (symbol (namespace sym))
        file (or file (fm/find-file-for-ns-on-cp ns-name))
        info (analyzed-data-of-def sym file)
        old-file-src (source-for-ns ns-name file)
        old-src (source-for-symbol sym file)]

    (if-not old-file-src
      (throw (Exception. (str "Cannot retrieve current source for " ns-name))))

    ; 1. update file and analyzed data
    (if write-to-file
      (when file
        (let [new-file-src (sf/updated-source sym info new-source old-src old-file-src)]
          (spit file new-file-src)
          (analyze-cljs-ns! ns-name)
          (if *compile?*
            (comp/compile-cljs-in-project
             (.getCanonicalPath (io/file "."))
             (:compiler-env (ensure-default-cljs-env)))))))
    
    ; 2. update runtime
    (eval-and-update-meta! sym new-source)
    ; (update-source-pos-of-defs-below! sym new-source old-src)
    
    ; 3. register change
    (let [change (record-change! sym new-source old-src)]
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
          (analyze-cljs-ns! ns-name)
          (if *compile?* (comp/compile-cljs-in-project
                          (.getCanonicalPath (io/file "."))
                          (:compiler-env (ensure-default-cljs-env))))))
      (let [diff (change-ns-in-runtime! ns-name new-source old-src file)
            change (record-change-ns! ns-name new-source old-src diff)]
        change))
    (throw (Exception. (str "Cannot retrieve current source for " ns-name)))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn ensure-ns-analyzed!
  [ns-name]
  (namespace-info ns-name))

(defn reset-cljs-analyzer
  []
  (reset! cljs-env {}))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment

 (reset! cljs-env {})
 (namespace-info 'rksm.test)

 (def file (first (rksm.cloxp-cljs.filemapping/cljs-files-in-cp-dirs)))
 (ana/forms-seq file)

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
;   (select-keys [:column :line :file])
   )


 (namespace-info 'rksm.test)

 (rksm.system-navigator.ns.internals/namespace-info *ns*)
 (ensure-default-cljs-env)

 (:cljs.analyzer/analyzed-cljs :cljs.analyzer/namespaces :options :js-dependency-index)

 (env/with-compiler-env (:compiler-env (ensure-default-cljs-env))
   @ana/namespaces)

 (java.util.Date. (.lastModified file))
 (-> (io/file "/Users/robert/clojure/cloxp-cljs/out") .exists)
 (-> (io/file "/Users/robert/clojure/cloxp-repl/out") .exists)

 (System/setProperty "user.dir" "/Users/robert/clojure/cloxp-cljs")


 )