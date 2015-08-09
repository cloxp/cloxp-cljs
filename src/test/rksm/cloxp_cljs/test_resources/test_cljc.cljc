(ns rksm.cloxp-cljs.test-resources.test-cljc)

(defn x-to-string
  [x]
  #?(:clj "foo" :cljs "bar"))