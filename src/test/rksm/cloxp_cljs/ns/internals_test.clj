(ns rksm.cloxp-cljs.ns.internals-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-cljs.ns.internals :refer :all]))

(defonce test-file "/Users/robert/clojure/cloxp-cljs/src/cljs/rksm/test.cljs")
(defonce orig-source "(ns rksm.test
  (:require [clojure.string :as s]))

(js/alert (.toUpperCase \"Running 3!\"))

(defn ^:export foo
  [x]
  (+ x 29))\n")
(defonce sep java.io.File/separator)

(defn source-state-fixture [test]
  (test)
  (spit test-file orig-source))

(use-fixtures :each source-state-fixture)

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest namespace-info-test
  
  (is (= {:interns
          [{:name "foo",
            :ns "rksm.test",
            :line 6,
            :column 1,
            :file test-file}],
          :file test-file,
          :imports nil,
          :requires {'clojure.string 'clojure.string, 's 'clojure.string},
          :uses nil,
          :excludes #{},
          :doc nil,
          :name 'rksm.test}
         (namespace-info 'rksm.test))))

(deftest source-for-symbol-test
  
  (is (= "(defn ^:export foo
  [x]
  (+ x 29))"
         (source-for-symbol 'rksm.test/foo))))

(deftest source-for-ns-test
  
  (is (= orig-source
         (source-for-ns 'rksm.test))))

(deftest change-ns-test

  (let [new-src "(ns rksm.test\n  ; (:require [clojure.string :as s])\n  )\n\n(js/alert \"Running!\")\n\n(defn foo\n  [x]\n  (+ x 24))"]
    (change-ns! 'rksm.test new-src true)
    (is (= new-src (slurp test-file))))
  
  )

(deftest change-def-test

  (let [new-src "(defn ^:export foo
  [x]
  (+ x 32))\n"
        expected-src "(ns rksm.test
  (:require [clojure.string :as s]))

(js/alert (.toUpperCase \"Running 3!\"))

(defn ^:export foo
  [x]
  (+ x 32))\n"]
    (change-def! 'rksm.test/foo new-src true)
    (is (= expected-src (slurp test-file))))
  
  )

(deftest has-cljs-core-analyzed
  (is (= {:name 'cljs.core/map, :fn-var true}
         (select-keys (symbol-info-for-sym 'cljs.core 'map) [:name :fn-var]))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest find-infos-about-symbols-in-ns
;   (symbol-info-for-sym 'rksm.test 's/join)
  )

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment

 (reset! rksm.cloxp-cljs.ns.internals/cljs-env {})
 (run-tests 'rksm.cloxp-cljs.ns.internals-test)

 (clojure.repl/pst *e 999)
 )