(ns rksm.cloxp-cljs.ns.analyzer-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-cljs.analyzer :as cljs]
            [rksm.cloxp-cljs.filemapping :as fm]
            [rksm.system-files :as sf]
            [clojure.java.io :as io]))

(defonce test-file "src/cljs/rksm/test.cljs")

(defonce orig-source "(ns rksm.test
  (:require [clojure.string :as s]
            [clojure.set :refer (union)]))

(js/alert (.toUpperCase \"Running 3!\"))

(defn ^:export foo
  [x]
  (+ x 29))\n")

(defonce sep java.io.File/separator)

(defn source-state-fixture [test]
  ; ensure analyzed:
  (cljs/namespace-info 'rksm.test)
  (test)
  (spit test-file orig-source)
  (reset! cljs/cljs-env {}))

(use-fixtures :each source-state-fixture)

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest namespace-file-discorvery
  (is (= (-> test-file io/file .getCanonicalPath)
        (-> 'rksm.test fm/find-file-for-ns-on-cp .getCanonicalPath))))

(deftest namespace-info-test

  (is (= {:interns
          [{:name 'rksm.test/foo,
            :ns 'rksm.test,
            :line 7,
            :column 1,
            :file (.getCanonicalPath (clojure.java.io/file test-file))}],
          :file (.getCanonicalPath (clojure.java.io/file test-file)),
          :imports nil,
          :requires {'clojure.string 'clojure.string
                    's 'clojure.string
                    'clojure.set 'clojure.set},
          :uses {'union 'clojure.set},
          :excludes #{},
          :doc nil,
          :name 'rksm.test}
        (cljs/namespace-info 'rksm.test))))

(deftest source-for-symbol-test

  (is (= "(defn ^:export foo
  [x]
  (+ x 29))\n"
        (cljs/source-for-symbol 'rksm.test/foo))))

(deftest source-for-ns-test

  (is (= orig-source
        (sf/source-for-ns 'rksm.test nil #"\.cljs$"))))

(deftest change-ns-test
  (let [new-src "(ns rksm.test\n  ; (:require [clojure.string :as s])\n  )\n\n(js/alert \"Running!\")\n\n(defn foo\n  [x]\n  (+ x 24))"]
    (cljs/change-ns! 'rksm.test new-src true)
    (is (= new-src (slurp test-file))))
  )

(deftest change-def-test

  (let [new-src "(defn ^:export foo
  [x]
  (+ x 32))\n"
        expected-src "(ns rksm.test
  (:require [clojure.string :as s]
            [clojure.set :refer (union)]))

(js/alert (.toUpperCase \"Running 3!\"))

(defn ^:export foo
  [x]
  (+ x 32))"]
    (cljs/change-def! 'rksm.test/foo new-src true)
    (is (= expected-src (slurp test-file)))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest find-infos-about-symbols-in-ns

  (testing "core"
    (is (= {:name 'cljs.core/map :ns 'cljs.core}
          (select-keys (cljs/var-info 'rksm.test 'map) [:name :ns]))))

  (testing "aliased"
    (is (= {:name 'clojure.string/join :ns 'clojure.string}
          (select-keys (cljs/var-info 'rksm.test 's/join) [:name :ns]))))

  (testing "refered"
    (is (= {:name 'clojure.set/union :ns 'clojure.set}
          (select-keys (cljs/var-info 'rksm.test 'union) [:name :ns]))))

  (testing "refered qualified"
    (is (= {:name 'clojure.set/union :ns 'clojure.set}
          (select-keys (cljs/var-info 'rksm.test 'clojure.set/union) [:name :ns]))))

  (testing "required"
    (is (= {:name 'clojure.set/difference :ns 'clojure.set}
          (select-keys (cljs/var-info 'rksm.test 'clojure.set/difference) [:name :ns]))))

  (testing "local"
    (is (= {:name 'rksm.test/foo :ns 'rksm.test}
          (select-keys (cljs/var-info 'rksm.test 'foo) [:name :ns]))))

  (testing "local qualified"
    (is (= {:name 'rksm.test/foo :ns 'rksm.test}
          (select-keys (cljs/var-info 'rksm.test 'rksm.test/foo) [:name :ns]))))
  )

; ; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (defonce test-file "/Users/robert/clojure/cloxp-cljs/src/cljs/rksm/test.cljs")
 (run-tests *ns*)
 (let [s (java.io.StringWriter.)] (binding [*test-out* s] (test-ns *ns*) (print (str s))))
 
 (reset! cljs/cljs-env {})
 (test-var #'cljs/change-def-test)
 (test-var #'find-infos-about-symbols-in-ns)
 
 (-> (cljs/ensure-default-cljs-env) :compiler-env deref)
 )
