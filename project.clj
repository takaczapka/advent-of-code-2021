(defproject advent-of-code-2021 "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.11.0"]]
  :main ^:skip-aot advent-of-code-2021.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot      :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
