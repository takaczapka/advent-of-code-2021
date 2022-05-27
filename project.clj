(defproject advent-of-code-2021 "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.11.0"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot      :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
