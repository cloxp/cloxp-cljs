(ns rksm.cloxp-cljs.ns.internals-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-cljs.ns.internals :refer :all]))

(defonce test-file "/Users/robert/clojure/cloxp-cljs/src/cljs/rksm/test.cljs")
(defonce orig-source "(ns rksm.test
  (:require [clojure.string :as s]
            [clojure.set :refer (union)]))

(js/alert (.toUpperCase \"Running 3!\"))

(defn ^:export foo
  [x]
  (+ x 29))\n")
(defonce sep java.io.File/separator)

(defn source-state-fixture [test]
  (ensure-ns-analyzed! 'rksm.test)
  (test)
  (spit test-file orig-source)
  (reset! rksm.cloxp-cljs.ns.internals/cljs-env {}))

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
          :requires {'clojure.string 'clojure.string
                     's 'clojure.string
                     'clojure.set 'clojure.set},
          :uses {'union 'clojure.set},
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

; (deftest change-ns-test
;   (let [new-src "(ns rksm.test\n  ; (:require [clojure.string :as s])\n  )\n\n(js/alert \"Running!\")\n\n(defn foo\n  [x]\n  (+ x 24))"]
;     (change-ns! 'rksm.test new-src true)
;     (is (= new-src (slurp test-file))))  
;   )

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
  (+ x 32))\n"]
    (change-def! 'rksm.test/foo new-src true)
    (is (= expected-src (slurp test-file)))))

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

 (reset! rksm.cloxp-cljs.ns.internals/cljs-env {})
 (run-tests 'rksm.cloxp-cljs.ns.internals-test)
 (test-var #'rksm.cloxp-cljs.ns.internals-test/change-def-test)

 (-> (rksm.cloxp-cljs.ns.internals/ensure-default-cljs-env) :compiler-env deref)
 )