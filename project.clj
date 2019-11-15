(defproject bluebell/utils "0.1.12-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :javac-options ["-Xlint:unchecked" "-Xlint:deprecation"
                  "-target" "1.8" "-source" "1.8"]
  :aot :all
  :dependencies [[org.clojure/clojure "1.10.0"]])
