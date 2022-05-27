(ns advent-of-code-2021.7
  (:require [clojure.test :refer :all]
            [advent-of-code-2021.common :as common]
            [clojure.string :as str]))

(defn abs [n] (max n (- n)))

(defn steps [n]
  (reduce + (range 1 (inc n))))

(defn calc-move [in pos]
  (apply + (map #(abs (- pos %)) in)))

(defn calc-move-2 [in pos]
  (apply + (map #(steps (abs (- pos %))) in)))

(defn all-moves [move-fn in]
  (let [max' (apply max in)
        min' (apply min in)]
    (map #(move-fn in %) (range min' max'))))

(deftest asas-test
  (let [_input (-> "7.txt"
               common/read-input
               first
               (str/split #","))
        input (map parse-long _input)]

    (is (= 347449 (apply min (all-moves calc-move input))))

    (is (= 98039527 (apply min (all-moves calc-move-2 input))))))