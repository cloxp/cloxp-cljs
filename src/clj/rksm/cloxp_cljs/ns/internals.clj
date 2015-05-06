(ns rksm.cloxp-cljs.ns.internals
  (:require [cljs.analyzer :as ana]
            [cljs.env :as env]
            [cljs.analyzer.api :as ana-api]
            [rksm.cloxp-cljs.filemapping :as fm]
            [rksm.cloxp-cljs.compilation :as comp]
            [rksm.system-files :as sf]
            [rksm.system-files.jar-util :as jar]
            [rksm.system-files.cljx :as sfx]
            [rksm.system-files.cljx.File :as sfx-file]
            [rksm.cloxp-source-reader.core :as src-rdr]
            [clojure.data.json :as json]
            [clojure.string :as s]
            [clojure.tools.reader :as tr]
            [cljs.closure :as cljsc]
            [cljs.tagged-literals :as cljs-literals]
            [clojure.java.io :as io])
  (:import (java.io LineNumberReader PushbackReader File)))

(declare ensure-ns-analyzed! analyzed-data-of-def analyze-cljs-ns!
         namespace-info)

(defonce cljs-env (atom {}))

(defn ensure-default-cljs-env
  []
  (if-let [e (:default @cljs-env)]
    e
    (env/ensure
     (let [env {:analyzer-env (atom (merge (ana/empty-env) {:ns 'cljs.user}))
                :compiler-env env/*compiler*}]
       (swap! cljs-env assoc :default env)
      (if-not (some-> env :compiler-env deref
                 :cljs.analyzer/namespaces (get 'cljs.core))
         (ensure-ns-analyzed! 'cljs.core))
       env))))


; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn source-for-symbol
  [sym & [file]]
  ; ensure we have analyzed data for ns of sym
  (let [ns-name (symbol (namespace sym))
        file (fm/find-file-for-ns-on-cp ns-name file)
        comp-env (or env/*compiler* (:compiler-env (ensure-default-cljs-env)))]
    (env/with-compiler-env comp-env
      (if-not (get-in (deref comp-env) [:cljs.analyzer/namespaces ns-name])
        (ensure-ns-analyzed! ns-name file))
      (if-let [file-data (some-> (analyzed-data-of-def sym file)
                           (select-keys [:column :line :file :name])
                           (update-in [:name] (comp symbol name)))]
        (binding [tr/*data-readers* cljs-literals/*cljs-data-readers*
                  tr/*alias-map* (apply merge ((juxt :requires :require-macros) file-data))
                  sfx-file/*output-mode* :cljx]
          (let [rdr (sf/source-reader-for-ns ns-name file #"\.clj(s|x)$")]
            (some->> [file-data]
              (src-rdr/add-source-to-interns-with-reader rdr)
              first :source)))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defn analyzed-data-of-def
  [sym & [file]]
  (let [ns-name (symbol (namespace sym))
        name (symbol (name sym))
        comp-env (or env/*compiler* (:compiler-env (ensure-default-cljs-env)))]
    (env/with-compiler-env comp-env
      (if-not (get-in ; in case ns data isn't there yet...
                      (deref comp-env)
                      [:cljs.analyzer/namespaces ns-name])
        (ensure-ns-analyzed! ns-name))
      (some-> (ensure-default-cljs-env)
        :compiler-env deref
        :cljs.analyzer/namespaces (get ns-name)
        :defs (get name)
        (assoc :ns ns-name)))))

(def ^{:doc "2015-03-23: there seems to be an issue with the line numbers
associated with interns, off by +1"} intern-line-offset -1)

(defn intern-info
  [{qname :name :as analyzed-def}]
  (-> analyzed-def
    (select-keys [:file :column :line])
    (assoc :ns (namespace qname) :name (name qname))
    (update-in [:line] + intern-line-offset)))

(defn symbol-info-for-macro
  [ns-name name]
  (some-> (ns-interns ns-name) (get name) meta))

(defn symbol-info-for-sym
  "find what we know about symbol in a given namespace. This is *not* asking
  for the meta data of a var, rather looking up what symbol is bound to what
  thing in a given namespaces"
  [ns-name sym & [file]]
  (cljs.env/with-compiler-env (or env/*compiler*
                                  (:compiler-env (ensure-default-cljs-env))
                                  (env/default-compiler-env))
    (let [local (-> (ana-api/empty-env)
                  (assoc :ns (namespace-info ns-name)))
          ref (ana-api/resolve local sym)
          info (cond
                 (nil? ref) nil
                 (= [:ns :name] (keys ref)) (ana-api/ns-resolve
                                             (:ns ref) (-> ref :name name symbol))
                 :default ref)]
      (some-> info (assoc :ns (-> info :name namespace symbol))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn- transform-namespace-data
  "for add all info cloxp needs. data comes
  from env/*compiler* :cljs.analyzer/namespaces"
  [data & [file]]
  (some-> data
    (select-keys [:name :doc :excludes :use :require :uses :requires :imports])
    (assoc :file (if file (str file)))
    (assoc :interns (->> (:defs data) vals (map intern-info) reverse))))

(defn namespace-info
  [ns-name & [file]]
  (let [file (or file (fm/find-file-for-ns-on-cp ns-name))]
    (cljs.env/with-compiler-env (or env/*compiler*
                                    (:compiler-env (ensure-default-cljs-env))
                                    (env/default-compiler-env))
      (some-> (or (ana-api/find-ns ns-name)
                  (analyze-cljs-ns! ns-name file))
        (transform-namespace-data file)))))

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

(defn analyze-cljs-ns!
  [ns-sym & [file]]
  ; have real clj namespaces represent cljs counterparts
  (if-not (find-ns ns-sym)
    (create-ns ns-sym))

  (let [cenv-atom (or env/*compiler*
                      (:compiler-env (ensure-default-cljs-env))
                      (env/default-compiler-env))
        file (fm/find-file-for-ns-on-cp ns-sym file)

        ; FIXME: right now jar files won't support the auto cljx translation,
        ; try to find cljs file instead
        file (if (and (jar/jar-url-string?
                       (-> (if (string? file) file (str (.getCanonicalPath file)))))
                      (sfx/cljx-file? file))
               (sf/file-for-ns ns-sym nil #"\.cljs")
               file)]
    (if-not file (throw (Exception. (str "Cannot find cljs or cljx file for namespace " ns-sym))))

    (env/with-compiler-env cenv-atom
      ; first pass: ensure that dependencies are analyzed
      (if-not (ana-api/find-ns ns-sym)
        (ana/no-warn (ana-api/analyze-file file {:cache-analysis false})))
      (ana-api/remove-ns ns-sym)
      ; second pass: there might be warnings
      (ana-api/analyze-file file {:cache-analysis false})
      (ana-api/find-ns ns-sym))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

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
        info (intern-info (analyzed-data-of-def sym file))
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
                      (:compiler-env (ensure-default-cljs-env)))))
    (let [diff (change-ns-in-runtime! ns-name new-source old-source file)
          change (record-change-ns! ns-name new-source old-source diff)]
      change)))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn ensure-ns-analyzed!
  [ns-name & [file]]
  (namespace-info ns-name file))

(defn reset-cljs-analyzer
  []
  (reset! cljs-env {}))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment

 (reset! cljs-env {})
 (namespace-info 'rksm.test)
)