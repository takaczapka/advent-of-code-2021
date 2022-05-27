(ns advent-of-code-2021.1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn ac-1 [m]
  (->> m
       (partition 2 1)
       (filter (fn [[a b]] (< a b)))
       count))

(defn read-input [file]
  (->
    (io/resource file)
    (slurp)
    (str/split #"\n")))

(comment
  (->> (read-input "1.txt")
       (map #(Integer/parseInt %))
       (ac-1)))

