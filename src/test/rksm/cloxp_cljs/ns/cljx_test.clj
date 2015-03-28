(ns rksm.cloxp-cljs.ns.cljx-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-cljs.filemapping :refer [find-file-for-ns-on-cp cp-dirs-with-cljs]]
            [rksm.cloxp-cljs.ns.internals :refer [ensure-ns-analyzed!
                                                  namespace-info
                                                   change-def!
                                                ;   source-for-symbol symbol-info-for-sym
                                                  ]]
            [rksm.system-files :as sf]
            [rksm.system-files.cljx :as sfx]
            [clojure.java.io :as io]))

(defonce test-file "src/test/rksm/cloxp_cljs/test_resources/test_cljx.cljx")

(defonce orig-source (slurp test-file))

(defn source-state-fixture [test]
  (sfx/enable-cljx-load-support!)
  (ensure-ns-analyzed! 'rksm.cloxp-cljs.test-resources.test-cljx)
  (test)
  (spit test-file orig-source)
  (reset! rksm.cloxp-cljs.ns.internals/cljs-env {}))

(use-fixtures :each source-state-fixture)

(deftest namespace-file-discorvery
  (is (= (-> test-file io/file .getCanonicalPath)
         (str (find-file-for-ns-on-cp 'rksm.cloxp-cljs.test-resources.test-cljx))))
  (is (= [(.getCanonicalPath (io/file "src/cljs/rksm/test.cljs"))
          (.getCanonicalPath (io/file "src/test/rksm/cloxp_cljs/test_resources/test_cljx.cljx"))]
         (->> (cp-dirs-with-cljs)
           vals
           (apply concat)
           (map str)
           (filter (partial re-find #"cloxp-cljs/"))))))

(deftest namespace-info-test
  (is (= {:interns
          [{:name "x-to-string",
            :ns "rksm.cloxp-cljs.test-resources.test-cljx",
            :line 3, :column 1,
            :file (.getCanonicalPath (clojure.java.io/file test-file))}],
          :file (.getCanonicalPath (clojure.java.io/file test-file)),
          :doc nil,
          :imports nil,
          :excludes #{},
          :requires nil,
          :uses nil,
          :name 'rksm.cloxp-cljs.test-resources.test-cljx}
         (namespace-info 'rksm.cloxp-cljs.test-resources.test-cljx))))

(deftest change-def-test

  (let [new-src "(defn x-to-string\n  [x]\n  \"foo\")"
        expected-src "(ns rksm.cloxp-cljs.test-resources.test-cljx)\n\n(defn x-to-string\n  [x]\n  \"foo\")"]
    (change-def! 'rksm.cloxp-cljs.test-resources.test-cljx/x-to-string new-src true)
    (is (= expected-src (slurp test-file)))))

(comment
 (deftest source-for-symbol-test

  (is (= "(defn ^:export foo
  [x]
  (+ x 29))\n"
         (source-for-symbol 'rksm.test/foo))))

(deftest source-for-ns-test

  (is (= orig-source
         (sf/source-for-ns 'rksm.test nil #"\.cljs$"))))

; (deftest change-ns-test
;   (let [new-src "(ns rksm.test\n  ; (:require [clojure.string :as s])\n  )\n\n(js/alert \"Running!\")\n\n(defn foo\n  [x]\n  (+ x 24))"]
;     (change-ns! 'rksm.test new-src true)
;     (is (= new-src (slurp test-file))))
;   )



; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest find-infos-about-symbols-in-ns

  (testing "core"
    (is (= {:name 'cljs.core/map :ns 'cljs.core}
           (select-keys (symbol-info-for-sym 'rksm.test 'map) [:name :ns]))))

  (testing "aliased"
    (is (= {:name 'clojure.string/join :ns 'clojure.string}
           (select-keys (symbol-info-for-sym 'rksm.test 's/join) [:name :ns]))))

  (testing "refered"
    (is (= {:name 'clojure.set/union :ns 'clojure.set}
           (select-keys (symbol-info-for-sym 'rksm.test 'union) [:name :ns]))))

  (testing "refered qualified"
    (is (= {:name 'clojure.set/union :ns 'clojure.set}
           (select-keys (symbol-info-for-sym 'rksm.test 'clojure.set/union) [:name :ns]))))

  (testing "required"
    (is (= {:name 'clojure.set/difference :ns 'clojure.set}
           (select-keys (symbol-info-for-sym 'rksm.test 'clojure.set/difference) [:name :ns]))))

  (testing "local"
    (is (= {:name 'rksm.test/foo :ns 'rksm.test}
           (select-keys (symbol-info-for-sym 'rksm.test 'foo) [:name :ns]))))

  (testing "local qualified"
    (is (= {:name 'rksm.test/foo :ns 'rksm.test}
           (select-keys (symbol-info-for-sym 'rksm.test 'rksm.test/foo) [:name :ns]))))
  )

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (run-tests *ns*)

 (reset! rksm.cloxp-cljs.ns.internals/cljs-env {})
 (test-var #'rksm.cloxp-cljs.ns.internals-test/change-def-test)

 (-> (rksm.cloxp-cljs.ns.internals/ensure-default-cljs-env) :compiler-env deref)
   ))
