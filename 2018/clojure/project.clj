(defproject clojure "0.1.0-SNAPSHOT"
  :description "Advent of Code 2018 solutions"
  :main main
  ; Downloaded from https://github.com/Z3Prover/z3/releases/tag/z3-4.8.4
  :resource-paths ["resources/com.microsoft.z3.jar"]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.finger-tree "0.0.3"]
                 [criterium "0.4.4"]])
