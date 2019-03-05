(defproject bluebell/utils "0.1.8"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :javac-options ["-Xlint:deprecation"]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "0.3.443"]]
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version"
                   "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag"]
                  ["deploy" "clojars"]])
