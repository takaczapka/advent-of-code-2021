(ns advent-of-code-2021.8
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn sort-string [s]
  (str/join (sort s)))

(defn read-input [i]
  (let [lines (str/split i #"\n")]
    (map
      #(map (fn [s]
              (map
                sort-string
                (str/split s #" ")))
            (str/split % #" \| "))
      lines)))

(defn common-with [s1 s2]
  (count (filter #(.contains s1 (str %)) (vec s2))))

(defn classify [ss]
  (let [one        (first (filter #(= 2 (count %)) ss))
        four       (first (filter #(= 4 (count %)) ss))
        normalised (map (fn [s] [s (count s) (common-with s one) (common-with s four)]) ss)]
    (into {} (map (fn [[s & rest]]
                    [s (cond
                         (= [2 2 2] rest) 1
                         (= [5 1 2] rest) 2
                         (= [5 2 3] rest) 3
                         (= [4 2 4] rest) 4
                         (= [5 1 3] rest) 5
                         (= [6 1 3] rest) 6
                         (= [3 2 2] rest) 7
                         (= [7 2 4] rest) 8
                         (= [6 2 4] rest) 9
                         (= [6 2 3] rest) 0
                         :default :YYY)])
                  normalised))))

(deftest aoc-8

  ; part 1
  (let [res
        (->>
          "8.txt"
          io/resource
          slurp
          read-input
          (map second)
          flatten
          (map count))]
    (is (= 416 (count (filter #(or (= % 2) (= % 4) (= % 3) (= % 7)) res)))))

  ; part 2
  (let [in (read-input (slurp (io/resource "8.txt")))]
    (is (= 1043697
           (apply +
                  (map
                    (fn [[left right]]
                      (let [m (classify left)]
                        (parse-long (str/join (map #(get m %) right)))))
                    in))))
    ))