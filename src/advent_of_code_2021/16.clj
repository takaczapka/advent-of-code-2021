(ns advent-of-code-2021.16
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [file]
  (->
    (io/resource file)
    (slurp)))

(def mapping
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(defn to-int [bits]
  (if (vector? bits)
    (BigInteger. (str/join bits) 2)
    (BigInteger. bits 2)))

;ID 4 represent a literal value
;Every other type of packet (any packet with a type ID other than 4) represent an operator that performs some calculation on one or more sub-packets contained within.

(defn calc-dec [id-type ss]
  (let [is (reverse ss)]
    (case id-type
      0 (apply + is)
      1 (apply * is)
      2 (apply min is)
      3 (apply max is)
      5 (if (> (first is) (second is)) 1 0)
      6 (if (< (first is) (second is)) 1 0)
      7 (if (= (first is) (second is)) 1 0))))

(defn type-4 [s]
  (let [ps (partition 5 (subs s 6))
        fs (take-while #(= \1 (first %)) ps)
        rs (drop 1 (first (drop (count fs) ps)))]
    [(to-int (str/join (concat (mapcat rest fs) rs))) (str/join (subs s (+ 6 (* 5 (inc (count fs))))))]))

(defn type-x-0-x [parser s]
  (let [id-type-bin       (subs s 3 6)
        id-type           (Integer/parseInt (str/join id-type-bin) 2)
        ; this is 0 for now
        length-of-packets (to-int (subs s 7 22))
        ssss              (subs s 22 (+ 22 length-of-packets))
        [vs more] (first (drop-while (fn [[_ b]]
                                       (not (or (= 0 (count b)) (= #{\0} (set b)))))
                                     (iterate (fn [[a b]]
                                                (let [[x y] (parser b)]
                                                  [(cons x a) y])
                                                ) [[] ssss])))]
    [(calc-dec id-type vs) (str/join more (subs s (+ 22 length-of-packets)))]))

(defn type-x-1-x [parser s]
  (let [id-type-bin          (subs s 3 6)
        id-type              (Integer/parseInt (str/join id-type-bin) 2)
        number-of-subpackets (to-int (subs s 7 18))
        ssss                 (subs s 18)                    ;; 11 is for now
        [vs more] (last (take (inc number-of-subpackets)    ; this is ilittle iffy
                              (iterate (fn [[a b]]
                                         (let [[x y] (parser b)]
                                           [(cons x a) y]))
                                       [[] ssss])))]
    [(calc-dec id-type vs) more]))

(defn give-me-type [s]
  (let [_           (subs s 0 3)
        id-type-bin (subs s 3 6)
        id-type     (Integer/parseInt (str/join id-type-bin) 2)]
    (if (= 4 id-type)
      :type-4
      (if (= "0" (subs s 6 7))
        :type-x-0
        :type-x-1))))

(defn parser [s]
  (if (str/blank? s)
    [[] ""]
    (case (give-me-type s)
      :type-4 (type-4 s)
      :type-x-0 (type-x-0-x parser s)
      :type-x-1 (type-x-1-x parser s))))

(defn do-calc [s]
  (let [ss (->> s
                (map mapping)
                (str/join))]
    (->> ss
         parser
         first)))

(deftest aoc-15
  (is (= 3 (do-calc "C200B40A82")))
  (is (= 54 (do-calc "04005AC33890")))
  (is (= 7 (do-calc "880086C3E88112")))
  (is (= 1 (do-calc "9C0141080250320F1802104A08")))
  ;
  (is (= 9 (do-calc "CE00C43D881120")))
  (is (= 1 (do-calc "D8005AC2A8F0")))                       ;D8005AC2A8F0 produces 1, because 5 is less than 15.
  (is (= 0 (do-calc "F600BC2D8F")))
  (is (= 0 (do-calc "9C005AC2F8F0")))

  (let [input (read-input "16.txt")]
    (is (= 2056021084691 (do-calc input)))))
