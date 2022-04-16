(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b] ; for b/git-count-revs
            [org.corfield.build :as bb]))

(def version (format "1.0.%s" (b/git-count-revs nil)))
(def lib 'aoc-2021/aoc-2021)
(def main 'aoc-2021.aoc-2021.main)

(defn uberjar
  "Create the uberjar"
  [opts]
  (println "Creating the uberjar")
  (-> opts
      (assoc :lib lib :version version :main main)
      (bb/clean)
      (bb/uber)))
