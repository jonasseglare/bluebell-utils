(defproject bluebell/utils "0.1.10-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :javac-options ["-Xlint:deprecation"]
  :aot :all
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "0.3.443"]])
