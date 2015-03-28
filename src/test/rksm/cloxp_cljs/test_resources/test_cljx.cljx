(ns rksm.cloxp-cljs.test-resources.test-cljx)

(defn x-to-string
  [x]
  (let [buf #+clj (StringBuilder.) #+cljs (gstring/StringBuffer.)]
    (.append buf "x is:")
    (.append buf (str x))))
