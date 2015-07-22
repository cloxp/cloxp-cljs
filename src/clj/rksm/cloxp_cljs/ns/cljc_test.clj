(ns rksm.cloxp-cljs.ns.cljc-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-cljs.filemapping :refer [find-file-for-ns-on-cp cp-dirs-with-cljs]]
            [rksm.cloxp-cljs.analyzer :refer [namespace-info
                                              change-def!
                                              change-ns!
                                              source-for-symbol
                                              var-info]]
            [rksm.system-files :as sf]
            [clojure.java.io :as io]))

(defonce test-file
  (-> "src/test/rksm/cloxp_cljs/test_resources/test_cljc.cljc" io/file .getCanonicalFile))

(defonce orig-source (slurp test-file))

(defonce test-ns-name 'rksm.cloxp-cljs.test-resources.test-cljc)

(defn source-state-fixture [test]
  (namespace-info 'rksm.cloxp-cljs.test-resources.test-cljc)
;   (source-for-symbol 'rksm.cloxp-cljs.test-resources.test-cljc/x-to-string)
  (test)
  (spit test-file orig-source)
  (reset! rksm.cloxp-cljs.analyzer/cljs-env {}))

(use-fixtures :each source-state-fixture)

(deftest namespace-file-discorvery
  (is (= (-> test-file .getCanonicalPath)
         (str (find-file-for-ns-on-cp 'rksm.cloxp-cljs.test-resources.test-cljc))))
  (is (= [(.getCanonicalPath (io/file "src/cljs/rksm/test.cljs"))
          (.getCanonicalPath (io/file "src/test/rksm/cloxp_cljs/test_resources/test_cljc.cljc"))]
         (->> (cp-dirs-with-cljs)
           vals
           (apply concat)
           (map str)
           (filter (partial re-find #"cloxp-cljs/"))))))

(deftest namespace-info-test
  (is (= {:interns
          [{:name 'rksm.cloxp-cljs.test-resources.test-cljc/x-to-string,
            :ns 'rksm.cloxp-cljs.test-resources.test-cljc,
            :line 3, :column 1,
            :file (.getCanonicalPath test-file)}],
          :file (.getCanonicalPath test-file),
          :doc nil,
          :imports nil,
          :excludes #{},
          :requires nil,
          :uses nil,
          :name 'rksm.cloxp-cljs.test-resources.test-cljc}
         (namespace-info 'rksm.cloxp-cljs.test-resources.test-cljc))))

(deftest source-for-symbol-test
  
  (is (= "(defn x-to-string
  [x]
  (let [buf #?(:clj (StringBuilder.) :cljs (gstring/StringBuffer.))]
    (.append buf \"x is:\")
    (.append buf (str x))))\n"
         (source-for-symbol (symbol (str test-ns-name) "x-to-string")))))

(deftest source-for-ns-test

  (is (= orig-source
         (sf/source-for-ns test-ns-name))))

(deftest find-infos-about-symbols-in-ns
  
  (testing "local"
    (is (= [(symbol (str test-ns-name) "x-to-string") test-ns-name]
          ((juxt :name :ns) (var-info test-ns-name 'x-to-string)))))
  
  (testing "local qualified"
    (is (= [(symbol (str test-ns-name) "x-to-string") test-ns-name]
          ((juxt :name :ns)
           (var-info test-ns-name
                     (symbol (str test-ns-name) "x-to-string"))))))) 

(deftest change-def-test
  (let [def-name (symbol (str test-ns-name) "x-to-string")
        new-def-src "(defn x-to-string\n  [x]\n  #?(:clj \"foo\" :cljs \"bar\"))"
        new-ns-src (str "(ns rksm.cloxp-cljs.test-resources.test-cljc)\n\n" new-def-src)]
    (change-def! def-name new-def-src true)
    (is (= new-ns-src (slurp test-file)))
    (is (= (str new-def-src "\n") (source-for-symbol def-name)))))

(deftest change-ns-test
  (let [def-name (symbol (str test-ns-name) "x-to-string")
        new-def-src "(defn x-to-string\n  [x]\n  #?(:clj \"foo\" :cljs \"bar\"))"
        new-ns-src (str "(ns rksm.cloxp-cljs.test-resources.test-cljc)\n\n" new-def-src)]
    (change-ns! test-ns-name new-ns-src true)
    (is (= new-ns-src (slurp test-file)))
    (is (= (str new-def-src "\n") (source-for-symbol def-name)))))
    
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (run-tests *ns*)
 (let [s (java.io.StringWriter.)] (binding [*test-out* s] (test-ns *ns*) (print (str s))))

 (reset! rksm.cloxp-cljs.analyzer/cljs-env {})
 (test-var #'rksm.cloxp-cljs.analyzer-test/change-def-test)
 
 (-> (rksm.cloxp-cljs.analyzer/ensure-default-cljs-env) :compiler-env deref)
 )
