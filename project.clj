(def ^:private test-file-pattern #".*_test.clj")

(defproject ray-tracer-challenge "0.1.0-SNAPSHOT"
  :description "The Ray Tracer Challenge"
  :url "http://server.fake/ray-tracer-challenge"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/math.numeric-tower "0.0.5"]]
  :main ^:skip-aot ray-tracer-challenge.putting-it-together.all-chapters
  :test-paths ["src"]
  :test-selectors {:default [(fn [ns] (.endsWith (str ns) "-test"))
                             (constantly true)]}
  :target-path "target/%s"
  :profiles {:uberjar {:aot      :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :jar-exclusions ~[test-file-pattern]
  :uberjar-exclusions ~[test-file-pattern])
