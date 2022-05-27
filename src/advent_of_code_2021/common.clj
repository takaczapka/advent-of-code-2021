(ns advent-of-code-2021.common
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [file]
  (-> (io/resource file)
      slurp
      (str/split #"\n")))