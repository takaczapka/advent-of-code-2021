(ns advent-of-code-2021.3
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [advent-of-code-2021.common :as common]))

(defn rotate [vs]
  (apply map vector vs))

(defn the-most-common [v]
  (if (< (* (count (filter #(= \1 %) v)) 2) (count v))
    \0
    \1))

(defn occurrences [v default k]
  (let [double-ones (* (count (filter #(= \1 %) v)) 2)
        all         (count v)]
    (cond
      (= double-ones all) default
      (< double-ones all) (if (= k :least) \1 \0)
      :else (if (= k :least) \0 \1))))

(defn most-common [v default]
  (occurrences v default :most))

(defn least-common [v default]
  (occurrences v default :least))

(defn calc-gamma-rate [vs]
  (->> (rotate vs)
       (map #(the-most-common %))
       str/join))

(defn bitstr->int [bit-str]
  (Integer/parseInt bit-str 2))

(defn bitarray->int [bit-array]
  (bitstr->int (str/join bit-array)))

(defn aoc-3-1 [i]
  (let [gamma-rate   (bitstr->int (calc-gamma-rate i))
        to-xor-with  (str/join (repeat (count (first i)) 1))
        epsilon-rate (bit-xor gamma-rate (bitstr->int to-xor-with))]
    (* gamma-rate epsilon-rate)))

; oxygen generator rating
(defn most-common-at-pos-or [vs n]
  (most-common (nth (rotate vs) n) \1))

(defn least-common-at-pos-or [vs n]
  (least-common (nth (rotate vs) n) \0))

(defn filter-by-occs [occurrences-f pos cs]
  (if (or (= (count cs) 1) (= pos (count (first cs))))
    (first cs)
    (let [selc (occurrences-f cs pos)]
      (filter-by-occs occurrences-f (inc pos) (filter #(= selc (nth % pos)) cs)))))

(defn aoc-3-2 [input]
  (let [data                    (map vec input)
        oxygen-generator-rating (bitarray->int (filter-by-occs most-common-at-pos-or 0 data))
        c02-scrubber-rating     (bitarray->int (filter-by-occs least-common-at-pos-or 0 data))]
    (* oxygen-generator-rating c02-scrubber-rating)))

(deftest aoc-3-2-test
  (let [data [[\1 \0 \0] [\1 \0 \1] [\0 \1 \0] [\1 \1 \0]]]
    (is (= \1 (most-common-at-pos-or data 0)))
    (is (= \1 (most-common-at-pos-or data 1)))
    (is (= \0 (most-common-at-pos-or data 2))))

  (let [data ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"]]
    (is (= 230 (aoc-3-2 data))))

  (is (= 1662846 (aoc-3-2 (common/read-input "3.txt")))))

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

  (is (= 2954600 (aoc-3-1 (common/read-input "3.txt")))))