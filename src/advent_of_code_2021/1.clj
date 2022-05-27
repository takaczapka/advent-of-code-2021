(ns advent-of-code-2021.1
  (:require [advent-of-code-2021.common :as common]
            [clojure.test :refer :all]))

(defn ac-1-1 [m]
  (->> m
       (partition 2 1)
       (filter #(< (first %) (second %)))
       count))

(defn ac-1-2 [m]
  (->> m
       (partition 3 1)
       (map (partial apply +))
       (partition 2 1)
       (filter #(< (first %) (second %)))
       count))

(deftest a-1-1-test
  (is (= 1301)
      (->> (common/read-input "1.txt")
           (map #(parse-long %))
           (ac-1-1))))

(deftest a-1-2-test
  (is (= 1346)
      (->> (common/read-input "1.txt")
           (map #(parse-long %))
           (ac-1-2))))

