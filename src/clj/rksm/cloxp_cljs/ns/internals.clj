(ns rksm.cloxp-cljs.ns.internals
  (:require [cljs.analyzer :as ana]
            [cljs.env :as env]
            [rksm.cloxp-cljs.filemapping :as fm]
            [rksm.cloxp-cljs.compilation :as comp]
            [rksm.system-files :as sf]
            [clojure.data.json :as json]
            [clojure.string :as s]
            [rksm.cloxp-source-reader.core :as src-rdr]
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
  (let [ns-name (symbol (namespace sym))] 
    (if-not (get-in
             (deref (or env/*compiler* (:compiler-env (ensure-default-cljs-env))))
             [:cljs.analyzer/namespaces ns-name])
      (ensure-ns-analyzed! ns-name)))
  (if-let [file-data (some-> (analyzed-data-of-def sym file)
                       (select-keys [:column :line :file :name])
                       (update-in [:name] (comp symbol name)))]
    (let [def-file (:file file-data)
          rdr (sf/source-reader-for-ns ns-name def-file #"\.cljs$")]
      (some->> [file-data]
        (src-rdr/add-source-to-interns-with-reader rdr)
        first :source))))

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

(def ^{:doc "2015-03-23: there seems to be an issue with the line numbers
associated with interns, off by +1"} intern-line-offset -1)

(defn intern-info
  [{qname :name :as analyzed-def}]
  (-> analyzed-def
    (select-keys [:file :column :line])
    (assoc :ns (namespace qname) :name (name qname))
    (update-in [:line] + intern-line-offset)))

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
  (let [file (or file (fm/find-file-for-ns-on-cp ns-name))]
    (if-let [data (analyze-cljs-ns! ns-name)
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
      (let [interns (->> (:defs data) vals (map intern-info) reverse)
            ; interns-2 (binding [*ns* (find-ns ns-name)
            ;                     tr/*data-readers* cljs.tagged-literals/*cljs-data-readers*
            ;                     tr/*alias-map* (apply merge ((juxt :requires :require-macros) data))
            ;                     ]
            ;             (src-rdr/add-source-to-interns-with-reader
            ;              (sf/source-reader-for-ns ns-name file)
            ;              interns {:file file}))
            ]
        (-> data
          (select-keys [:name :doc :excludes :use :require :uses :requires :imports])
          (assoc :file (if file (str file)))
          (assoc :interns interns))))))

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
  [ns-sym & [file]]
  (create-ns ns-sym)
  (let [cenv-atom (or env/*compiler* (:compiler-env (ensure-default-cljs-env)) (env/default-compiler-env))
        file (or file (fm/find-file-for-ns-on-cp ns-sym))
        cljs-ana-file (cond
                        (instance? java.io.File file) file
                        (or (nil? file)
                            (and (string? file) (re-find #"^jar:" file))) (sf/ns-name->rel-path ns-sym ".cljs")
                        :default (io/file file))
        ; cljx? (and cljs-ana-file (re-find #"\.cljx$" (str cljs-ana-file)))
        ]
    (env/with-compiler-env cenv-atom
      (env/ensure
       ; first pass: ensure that dependencies are analyzed
       (if-not (-> env/*compiler* deref :cljs.analyzer/namespaces (get ns-sym))
         (ana/no-warn (cljs.analyzer/analyze-file cljs-ana-file {:cache-analysis false})))
       (swap! env/*compiler*
              (fn [cenv]
                (update-in cenv [:cljs.analyzer/namespaces] #(dissoc % ns-sym))
                (update-in cenv [:cljs.analyzer/analyzed-cljs] #(dissoc % cljs-ana-file))))
       ; second pass: there might be warnings
       (cljs.analyzer/analyze-file cljs-ana-file {:cache-analysis false})
       (-> env/*compiler* deref
         :cljs.analyzer/namespaces (get ns-sym))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(def ^:dynamic *compile?* true)

(defn update-source-file!
  [sym new-source old-src file]
  (let [info (analyzed-data-of-def sym file)
        ns-sym (symbol (namespace sym))
        file (or file (fm/find-file-for-ns-on-cp ns-sym))
        old-file-src (slurp file)
        new-file-src (src-rdr/updated-source sym info new-source old-src old-file-src)]
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
  (let [ns-name (symbol (namespace sym))
        file (or file (fm/find-file-for-ns-on-cp ns-name))
        info (intern-info (analyzed-data-of-def sym file))
        old-file-src (sf/source-for-ns ns-name file #".cljs$")
        old-src (source-for-symbol sym file)]

    (if-not old-file-src
      (throw (Exception. (str "Cannot retrieve current source for " ns-name))))

    ; 1. update file and analyzed data
    (if write-to-file
      (when file
        (let [new-file-src (src-rdr/updated-source sym info new-source old-src old-file-src)]
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
  (if-let [old-src (sf/source-for-ns ns-name file #".cljs$")]
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

 (def file (first (rksm.system-files/find-namespaces-on-cp #"\.cljs$")))
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