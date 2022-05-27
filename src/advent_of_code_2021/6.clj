(ns advent-of-code-2021.6
  (:require [clojure.test :refer :all]
            [advent-of-code-2021.common :as common]
            [clojure.string :as str]))

(defn lf [fs]
  (reduce (fn [acc f]
            (if (> f 0)
              (conj acc (dec f))
              (conj acc 6 8))) [] fs))

(defn all-lf [init days]
  (count (first (drop days (iterate lf init)))))

(defn fish-frequency [fs]
  (let [m (frequencies fs)]
    (map #(get m % 0) (range 9))))

(defn next-day [m]
  (let [[a-0 a-1 a-2 a-3 a-4 a-5 a-6 a-7 a-8] m]
    [a-1 a-2 a-3 a-4 a-5 a-6 (+ a-7 a-0) a-8 a-0]))

(defn fish-generations [init days]
  (let [generations (->> init
                         fish-frequency
                         (iterate next-day))]
    (apply + (nth generations days))))

(deftest aoc-6-test

  (is (= 5934 (all-lf [3, 4, 3, 1, 2] 80)))

  (let [_input (-> "6.txt"
                   common/read-input
                   first
                   (str/split #","))
        input  (map parse-long _input)]
    (is (= 1609058859115 (fish-generations
                           input
                           256)))))