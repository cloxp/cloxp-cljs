(ns rksm.cloxp-cljs.analyzer
  (:require [cljs.analyzer :as ana]
            [cljs.env :as env]
            [cljs.analyzer.api :as ana-api]
            [cljs.repl]
            [rksm.cloxp-cljs.filemapping :as fm]
            [rksm.cloxp-cljs.compilation :as comp]
            [rksm.system-files :as sf]
            [rksm.system-files.jar-util :as jar]
            [rksm.cloxp-source-reader.core :as src-rdr]
            [clojure.data.json :as json]
            [clojure.string :as s]
            [clojure.tools.reader :as tr]
            [cljs.closure :as cljsc]
            [cljs.tagged-literals :as cljs-literals]
            [clojure.java.io :as io])
  (:import (java.io LineNumberReader PushbackReader File)))

(declare namespace-info analyze-cljs-ns! ensure-ns-analyzed!)

(defonce cljs-env (atom {}))

(comment
 (reset! cljs-env {}))

(defn ensure-default-cljs-env
  []
  (if-let [e (:default @cljs-env)]
    e
    (let [comp-env (let [ups-deps (cljsc/get-upstream-deps)
                         opts {:ups-libs (:libs ups-deps)
                               :ups-foreign-libs (:foreign-libs ups-deps)}]
                     (env/default-compiler-env opts))
          env {:analyzer-env (atom (merge (ana/empty-env) {:ns 'cljs.user}))
               :compiler-env comp-env}]
      (swap! cljs-env assoc :default env)
      (if-not (some-> env :compiler-env deref
                :cljs.analyzer/namespaces (get 'cljs.core))
        (ensure-ns-analyzed! 'cljs.core))
      env)))

(defn comp-env
  []
  (or env/*compiler*
      (:compiler-env (ensure-default-cljs-env))))

(defn ana-env
  []
  (or @(:analyzer-env (ensure-default-cljs-env))
      (ana/empty-env)))

(defmacro with-compiler
  [& body]
  `(env/with-compiler-env (comp-env) ~@body))

(defn clean
  []
  (reset! cljs-env {}))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(def ^{:doc "2015-03-23: there seems to be an issue with the line numbers
associated with interns, off by +1"} intern-line-offset 0)

(defn- transform-var-info
  "subset of analyzed data returned by var-info, for use in tooling"
  [{qname :name :as analyzed-def} & [keys]]
  (let [data (-> analyzed-def
               (assoc :ns (-> qname namespace symbol))
               (update-in [:line] + intern-line-offset))]
    (if keys (select-keys data keys) data)))

(defn var-info
  "find what we know about symbol in a given namespace. This is *not* asking
  for the meta data of a var, rather looking up what symbol is bound to what
  thing in a given namespaces"
  [ns-name sym & [file keys]]
  (with-compiler
    (let [ana-env (assoc (ana-env) :ns (ana-api/find-ns ns-name))
          ref (ana-api/resolve ana-env sym)
          info (cond
                 (nil? ref) nil
                 (and (map? ref)
                      (= #{:ns :name} (.keySet ref))) (ana-api/ns-resolve
                                                       (:ns ref) (-> ref :name name symbol))
                 :default ref)]
      (some-> info (transform-var-info keys)))))

(defn source-for-symbol
  [sym & [file]]
  (let [ns-name (-> sym namespace symbol)
        sym-name (-> sym name symbol)
        file (fm/find-file-for-ns-on-cp ns-name file)]
    ; 1.
    (ensure-ns-analyzed! ns-name file)
    ; 2.
    (if-let [file-data (var-info ns-name sym-name
                                 file [:column :line :file :name])]
      (binding [tr/*data-readers* cljs-literals/*cljs-data-readers*
                tr/*alias-map* (apply merge ((juxt :requires :require-macros) file-data))]
        (some->> [file-data]
          (src-rdr/add-source-to-interns-with-reader
           (sf/source-reader-for-ns ns-name file #"\.clj(s|x|c)$"))
          first :source)))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn analyze-cljs-ns!
  [ns-sym & [file]]
  ; have real clj namespaces represent cljs counterparts
  (if-not (find-ns ns-sym)
    (create-ns ns-sym))

  (if-let [file (fm/find-file-for-ns-on-cp ns-sym file)]
    (with-compiler
      ; first pass: ensure that dependencies are analyzed
      (if-not (ana-api/find-ns ns-sym)
        (ana/no-warn (ana-api/analyze-file file {:cache-analysis false})))
      (ana-api/remove-ns ns-sym)
      ; second pass: there might be warnings
      (ana-api/analyze-file file {:cache-analysis false})
      (ana-api/find-ns ns-sym))
    (throw (Exception. (str "Cannot find cljs or cljc file for namespace " ns-sym)))))

(defn ensure-ns-analyzed!
  [ns-name & [file]]
  (namespace-info ns-name file))

(defn reset-cljs-analyzer
  []
  (reset! cljs-env {}))

(defn- transform-namespace-data
  "for add all info cloxp needs. data comes
  from env/*compiler* :cljs.analyzer/namespaces"
  [data & [file]]
  (some-> data
    (select-keys [:use-macros :excludes :macros :name :imports :requires :uses :require-macros :doc])
    (assoc :file (if file (str file)))
    (assoc :interns (->> (:defs data) vals
                      (map #(transform-var-info % [:macro :doc :file :line :column :name :ns]))
                      reverse))))

(defn namespace-info
  [ns-name & [file]]
  (let [file (or file (fm/find-file-for-ns-on-cp ns-name))]
    (with-compiler
      (some-> (or (ana-api/find-ns ns-name)
                  (analyze-cljs-ns! ns-name file))
        (transform-namespace-data file)))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; namespace/var data via json interface

(defn- stringify [obj]
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

(defn- jsonify
  [obj]
  (json/write-str obj
                  :key-fn #(if (keyword? %) (name %) (str %))
                  :value-fn (fn [_ x] (stringify x))))

(defn var-info->json [ns-name sym & [file]]
  (jsonify (var-info ns-name sym
                     file [:macro :doc :file :line :column :name :ns])))

(defn namespace-info->json [ns & [file-path]]
  (jsonify (namespace-info ns file-path)))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; change namespaces and vars

(def ^:dynamic *compile?* true)

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
  (let [ns-name (symbol (namespace sym))
        file (fm/find-file-for-ns-on-cp ns-name file)
        info (var-info ns-name (-> sym name symbol)
                       file [:file :column :line :ns])
        old-file-src (sf/source-for-ns ns-name file #".cljs$")
        old-src (source-for-symbol sym file)]

    (if-not file
      (throw (Exception. (str "Cannot retrieve cljs file for " ns-name))))

    (if-not old-file-src
      (throw (Exception. (str "Cannot retrieve current source for " ns-name))))

    ; 1. update file and analyzed data
    (when write-to-file
      (let [new-file-src (src-rdr/updated-source sym info new-source old-src old-file-src)]
        (spit file new-file-src)
        (analyze-cljs-ns! ns-name file)
        (if *compile?*
          (comp/compile-cljs-in-project
           ns-name file (.getCanonicalPath (io/file "."))
           new-file-src old-file-src
           (:compiler-env (ensure-default-cljs-env))))))

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
  (let [file (fm/find-file-for-ns-on-cp ns-name file)
        old-source (sf/source-for-ns ns-name file #".clj(x|s|c)$")]
    (when-not file
      (throw (Exception. (str "Cannot retrieve cljs file for " ns-name))))
    (when-not old-source
      (throw (Exception. (str "Cannot retrieve current source for " ns-name))))
    (when write-to-file
      (spit file new-source)
      (analyze-cljs-ns! ns-name file)
      (if *compile?* (comp/compile-cljs-in-project
                      ns-name file (.getCanonicalPath (io/file "."))
                      new-source old-source
                      (comp-env))))
    (let [diff (change-ns-in-runtime! ns-name new-source old-source file)
          change (record-change-ns! ns-name new-source old-source diff)]
      change)))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment

 (reset! cljs-env {})
 (namespace-info 'rksm.test)
)
