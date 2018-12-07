(defproject advent-of-code-2018 "0.0.1-SNAPSHOT"
  :description "Code for solving the 'Advent of Code 2018' problem set."
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [is-prime "0.1.0"]
                 [org.jgrapht/jgrapht-core "1.3.0"]]
  :javac-options ["-target" "1.8" "-source" "1.8" "-Xlint:-options"]
  :aot [advent-of-code-2018.core])

