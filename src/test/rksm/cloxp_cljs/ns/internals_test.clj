(ns rksm.cloxp-cljs.ns.internals-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-cljs.ns.internals :refer :all]))

(defonce test-file "/Users/robert/clojure/cloxp-cljs/src/cljs/rksm/test.cljs")
(defonce orig-source (slurp test-file))
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
            :line 7,
            :column 1,
            :file test-file}],
          :file test-file,
          :imports nil,
          :requires nil,
          :uses nil,
          :excludes #{},
          :doc nil,
          :name 'rksm.test}
         (namespace-info 'rksm.test))))

(deftest source-for-symbol-test
  
  (is (= "(defn foo
  [x]
  (+ x 23))"
         (source-for-symbol 'rksm.test/foo))))

(deftest source-for-ns-test
  
  (is (= "(ns rksm.test\n  ; (:require [clojure.string :as s])\n  )\n\n(js/alert \"Running!\")\n\n(defn foo\n  [x]\n  (+ x 23))"
         (source-for-ns 'rksm.test))))

(deftest change-ns-test

  (let [new-src "(ns rksm.test\n  ; (:require [clojure.string :as s])\n  )\n\n(js/alert \"Running!\")\n\n(defn foo\n  [x]\n  (+ x 24))"]
    (change-ns! 'rksm.test new-src true)
    (is (= new-src (slurp test-file))))
  
  )

(deftest change-def-test

  (let [new-src "(defn foo\n  [x]\n  (+ x 29))"
        expected-src "(ns rksm.test\n  ; (:require [clojure.string :as s])\n  )\n\n(js/alert \"Running!\")\n\n(defn foo\n  [x]\n  (+ x 29))"]
    (change-def! 'rksm.test/foo new-src true)
    (is (= expected-src (slurp test-file))))
  
  )

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest find-infos-about-symbols-in-ns
;   (symbol-info-for-sym 'rksm.test 's/join)
  )

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 123
 (run-tests 'rksm.cloxp-cljs.ns.internals-test)
 (clojure.repl/pst *e 999)
 )