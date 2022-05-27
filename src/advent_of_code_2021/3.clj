(ns advent-of-code-2021.3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(defn rotate [vs]
  (apply map vector vs))

(defn tmc [v]
  (if (< (count (filter #(= \1 %) v)) (count (filter #(= \0 %) v)))
    \0
    \1))

(defn tmc-or [v default]
  (let [ones  (count (filter #(= \1 %) v))
        zeros (count (filter #(= \0 %) v))]
    (if (= ones zeros)
      default
      (if (< ones zeros)
        \0
        \1))))

(defn tlc-or [v default]
  (let [ones  (count (filter #(= \1 %) v))
        zeros (count (filter #(= \0 %) v))]
    (if (= ones zeros)
      default
      (if (< ones zeros)
        \1
        \0))))

(defn do-it [vs]
  (->> (rotate vs)
       (map #(tmc %))
       str/join))

(defn as-int [bit-str]
  (Integer/parseInt bit-str 2))

(defn to-int [bit-array]
  (Integer/parseInt (str/join bit-array) 2))

(defn read-input [file]
  (->
    (io/resource file)
    (slurp)
    (str/split #"\n")))

(defn aoc-3-1 [i]
  (let [g-1         (as-int (do-it i))
        to-xor-with (str/join (repeat (count (first i)) 1))
        g-2         (bit-xor g-1 (as-int to-xor-with))]
    (* g-1 g-2)))



; oxygen generator rating

(defn most-common-at-pos-or [vs n]
  (tmc-or (nth (rotate vs) n) \1))

(defn least-common-at-pos-or [vs n]
  (tlc-or (nth (rotate vs) n) \0))

(defn fffiltr [ff pos cs]
  (if (or (= (count cs) 1) (= pos (count (first cs))))
    (first cs)
    (let [selc (ff cs pos)]
      (fffiltr ff (inc pos) (filter #(= selc (nth % pos)) cs)))))

(defn aoc-3-2 [input]
  (let [data (map vec input)
        g-1 (to-int (fffiltr most-common-at-pos-or 0 data))
        g-2 (to-int (fffiltr least-common-at-pos-or 0 data))
        _ (prn g-2)]
    (* g-1 g-2)))

(deftest aoc-2-2-test
  (let [data [[\1 \0 \0] [\1 \0 \1] [\0 \1 \0] [\1 \1 \0]]]
    (is (= \1 (most-common-at-pos-or data 0)))
    (is (= \1 (most-common-at-pos-or data 1)))
    (is (= \0 (most-common-at-pos-or data 2))))


  (let [data ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"]]
    (is (= 230 (aoc-3-2 data))))

  (is (= 1662846 (aoc-3-2 (read-input "3.txt"))))
  )




(comment
  (to-int "01001")

  (Integer/parseInt "0101" 2)

  )





(deftest aoc-3-1-test
  (is (= 198 (aoc-3-1 ["00100"
                       "11110"
                       "10110"
                       "10111"
                       "10101"
                       "01111"
                       "00111"
                       "11100"
                       "10000"
                       "11001"
                       "00010"
                       "01010"])))

  (is (= 2954600 (aoc-3-1 (read-input "3.txt")))))