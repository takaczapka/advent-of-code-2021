(ns advent-of-code-2021.8
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))











(defn do-the-mapping [i p-mapping]
  (map (fn [p] (map (fn [v] (replace p v)) i)) p-mapping))


(def valid (set (to-dd "abcefg cf acdeg acdeg acdfg bcdf abdfg acf abcdefg abcdfg")))

(defn is-valid [c]
  (prn (set c))
  ;(prn "valid" valid)
  (set/subset? (set c) valid)
  )

(defn read-input [i]
  (let [lines (str/split i #"\n")]
    (map #(map (fn [s] (str/split s #" ")) (str/split % #" \| ")) lines)
    )
  )


(defn common-with [s1 s2]
  (count (filter #(.contains s1 (str %)) (vec s2))))


(defn aaaa [ss]
  (let [one  (first (filter #(= 2 (count %)) ss))
        four (first (filter #(= 4 (count %)) ss))
        a    (map (fn [s] [s (count s) (common-with s one) (common-with s four)]) ss)]
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
                         :default :YYY
                         )]) a))))

(defn sort-string [s]
  (str/join (sort s)))

(defn bbbb [ss to-enc]
  (prn "vvvv" (aaaa (map sort-string ss)))
  (prn "vvvv" (sort-string to-enc))

  (get (aaaa (map sort-string ss)) (sort-string to-enc))

  )

(deftest aoc-8

  ; part 1
  ;(let [in "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
  ;
  ;      ]
  ;
  ;  (let [res (map count (flatten (read-input (slurp (io/resource "8.txt")))))]
  ;    ;(prn res)
  ;    (prn (count (filter #(or (= % 2) (= % 4) (= % 3) (= % 7)) res)))
  ;    ))

  (prn (read-input (slurp (io/resource "8.txt"))))

  ; part 2
  (let [in (read-input (slurp (io/resource "8.txt")))]
    (prn (apply + (map (fn [[left right]] (let [m (aaaa (map sort-string left))
                                  ]
                                   (Integer/parseInt (str/join (map #(get m (sort-string %)) right)))
                              )) in)))


    )

  )