(ns rksm.test
  (:require [clojure.string :as s]))

(js/alert (.toUpperCase "Running 3!"))

(defn ^:export foo
  [x]
  (+ x 29))
