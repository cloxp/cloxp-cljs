(ns rksm.cloxp-cljs.test-resources.test-cljx)

(defn x-to-string
  [x]
  (let [buf #+clj (StringBuilder.) #+cljs (gstring/StringBuffer.)]
    (.append buf "x is:")
    (.append buf (str x))))

(reify
  #+clj clojure.lang.IFn
  #+cljs cljs.core.IFn
  (invoke [_ x] (inc x)))