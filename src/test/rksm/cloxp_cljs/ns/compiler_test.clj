(ns rksm.cloxp-cljs.ns.compiler-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-cljs.analyzer :as ana]
            [rksm.cloxp-cljs.compilation :as comp]
            [clojure.java.io :as io]
            [fs.core]))

(defonce test-file "src/cljs/rksm/test.cljs")

(defonce orig-source "(ns rksm.test
  (:require [clojure.string :as s]
            [clojure.set :refer (union)]))

(js/alert (.toUpperCase \"Running 3!\"))

(defn ^:export foo
  [x]
  (+ x 29))\n")

(defn source-state-fixture [test]
  (fs.core/delete-dir "./cloxp-cljs-build")
  (test)
  (spit test-file orig-source))

(use-fixtures :each source-state-fixture)

(deftest compile-and-recompile 

  (let [file (.getCanonicalFile (io/file test-file))
        content (slurp file)
        new-content (clojure.string/replace content #"Running 3" "Running 4")
        comp-env (:compiler-env (ana/ensure-default-cljs-env))
        
        opts (comp/default-build-options ".")
        out-dir (:output-dir opts)
        js-file (cljs.build.api/target-file-for-cljs-ns 'rksm.test out-dir)
        cljs-file (io/file (clojure.string/replace (str js-file) #"\.js$" ".cljs"))
        
        ]
    
    (testing "basic compile works"
      (comp/compile-cljs-in-project 'rksm.test file "." content content comp-env)
      (is (.exists js-file))
      (is (.exists cljs-file))
      (is (.contains (slurp js-file) "Running 3"))
      (is (.contains (slurp cljs-file) "Running 3")))
    
    (testing "recompile works"
      ; note: compilation does not change the source file
      (spit file new-content)
      (comp/compile-cljs-in-project 'rksm.test file "." new-content content comp-env)
      (is (.exists js-file))
      (is (.exists cljs-file))
      (is (.contains (slurp js-file) "Running 4"))
      (is (.contains (slurp cljs-file) "Running 4")))))


(comment

 (let [s (java.io.StringWriter.)]
   (binding [*test-out* s]
     (test-ns *ns*)
     (print (str s))))
 
 )
