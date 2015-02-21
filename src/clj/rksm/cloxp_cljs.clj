(ns rksm.cloxp-cljs
  (:require [rksm.cloxp-cljs.filemapping :refer (find-cljs-namespaces-on-cp)]
            [rksm.cloxp-cljs.ns.internals :refer (find-cljs-namespaces-in-env)]))

(defn find-cljs-namespaces
  []
  (distinct 
   (concat 
    (find-cljs-namespaces-in-env)
    (find-cljs-namespaces-on-cp))))

(comment
 (clojure.data.json/write-str (sort (map str (rksm.cloxp-cljs/find-cljs-namespaces))))
 (find-cljs-namespaces)
 )