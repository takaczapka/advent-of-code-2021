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

; version 3
; type id 3


(defn to-int [bits]
  (if (vector? bits)
    (BigInteger. (str/join bits) 2)
    (BigInteger. bits 2)))

;;
;(defn type-4 [s]
;  (let [v           (subs s 0 3)
;        id-type-bin (subs s 3 6)
;        id-type     (Integer/parseInt (str/join id-type-bin) 2)
;        _ (prn :type-4 (subs s 6))
;        _ (prn :v (to-int v))
;        ps          (partition 5 (subs s 6))
;        fs          (take-while #(= \1 (first %)) ps)
;        rs          (drop 1 (first (drop (count fs) ps)))]
;    ;[(str/join (concat (mapcat rest fs) rs)) (str/join (subs s (+ 6 (* 5 (inc (count fs))))))]
;    [v (str/join (subs s (+ 6 (* 5 (inc (count fs))))))]
;    ))
;
;(defn type-x-0-x [parser s]
;  (let [v                 (subs s 0 3)
;        id-type-bin       (subs s 3 6)
;        id-type           (Integer/parseInt (str/join id-type-bin) 2)
;        ; this is 0 for now
;
;        length-of-packets (to-int (subs s 7 22))
;        _                 (prn :length-of-packets length-of-packets)
;        ssss              (subs s 22 (+ 22 length-of-packets))
;        _ (prn :type-x-0-x ssss)
;        _ (prn :v (to-int v))
;        [vs more]               (first (drop-while (fn [[_ b]]
;                                               (not (or (= 0 (count b)) (= #{\0} (set b)))))
;                                             (iterate (fn [[a b]]
;                                                        (let [[x y] (parser b)]
;                                                          [(cons x a) y])
;                                                        ) [[] ssss])))]
;    [(cons v vs) (str/join more (subs s (+ 22 length-of-packets)) )]))
;
;(defn type-x-1-x [parser s]
;  (let [v                    (subs s 0 3)
;        id-type-bin          (subs s 3 6)
;        id-type              (Integer/parseInt (str/join id-type-bin) 2)
;        ; this is 1 for now
;
;        ;_ (prn :s s)
;        number-of-subpackets (to-int (subs s 7 18))
;        _                    (prn :number-of-subpackets number-of-subpackets)
;        ssss                 (subs s 18 ) ;; 11 is for now
;        _ (prn :type-x-1-x ssss)
;        _ (prn :v (to-int v))
;
;        [vs more]                  (last (take (inc number-of-subpackets) ; this is ilittle iffy
;                                         (iterate (fn [[a b]]
;                                                    (let [[x y] (parser b)]
;                                                      [(cons x a) y])
;                                                    ) [[] ssss])))
;        ]
;    [(cons v vs) more]))
;;

;ID 4 represent a literal value
;Every other type of packet (any packet with a type ID other than 4) represent an operator that performs some calculation on one or more sub-packets contained within.

(defn calc-dec [id-type ss]
  (prn :id-type id-type)
  (prn :ss ss)

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
  (let [v           (subs s 0 3)
        id-type-bin (subs s 3 6)
        id-type     (Integer/parseInt (str/join id-type-bin) 2)
        _ (prn :type-4 (subs s 6))
        _ (prn :v (to-int v))
        ps          (partition 5 (subs s 6))
        fs          (take-while #(= \1 (first %)) ps)
        rs          (drop 1 (first (drop (count fs) ps)))]
    ;[(str/join (concat (mapcat rest fs) rs)) (str/join (subs s (+ 6 (* 5 (inc (count fs))))))]
    [(to-int (str/join (concat (mapcat rest fs) rs))) (str/join (subs s (+ 6 (* 5 (inc (count fs))))))]
    ))



(defn type-x-0-x [parser s]
  (let [v                 (subs s 0 3)
        id-type-bin       (subs s 3 6)
        id-type           (Integer/parseInt (str/join id-type-bin) 2)
        ; this is 0 for now

        length-of-packets (to-int (subs s 7 22))
        _                 (prn :length-of-packets length-of-packets)
        ssss              (subs s 22 (+ 22 length-of-packets))
        _ (prn :type-x-0-x ssss)
        _ (prn :v (to-int v))
        [vs more]               (first (drop-while (fn [[_ b]]
                                               (not (or (= 0 (count b)) (= #{\0} (set b)))))
                                             (iterate (fn [[a b]]
                                                        (let [[x y] (parser b)]
                                                          [(cons x a) y])
                                                        ) [[] ssss])))]
    [(calc-dec id-type vs) (str/join more (subs s (+ 22 length-of-packets)) )]))

(defn type-x-1-x [parser s]
  (let [v                    (subs s 0 3)
        id-type-bin          (subs s 3 6)
        id-type              (Integer/parseInt (str/join id-type-bin) 2)
        ; this is 1 for now

        ;_ (prn :s s)
        number-of-subpackets (to-int (subs s 7 18))
        _                    (prn :number-of-subpackets number-of-subpackets)
        ssss                 (subs s 18 ) ;; 11 is for now
        _ (prn :type-x-1-x ssss)
        _ (prn :v (to-int v))

        [vs more]                  (last (take (inc number-of-subpackets) ; this is ilittle iffy
                                         (iterate (fn [[a b]]
                                                    (let [[x y] (parser b)]
                                                      [(cons x a) y])
                                                    ) [[] ssss])))
        ]
    [(calc-dec id-type vs) more]))

(defn give-me-type [s]
  ;(prn :give-me s)
  (let [v                    (subs s 0 3)
        id-type-bin          (subs s 3 6)
        id-type              (Integer/parseInt (str/join id-type-bin) 2)]
    (if (= 4 id-type)
      :type-4
      (if (= "0" (subs s 6 7))
        :type-x-0
        :type-x-1))
    )
  )

(defn parser [s]
  (if (str/blank? s)
    [[] ""]
    (case (give-me-type s)
      :type-4 (type-4 s)
      :type-x-0 (type-x-0-x parser s)
      :type-x-1 (type-x-1-x parser s)
      ))
  )

(defn do-calc [s]
  (let [ss (->> s
                (map mapping)
                (str/join))
        _ (prn :input ss)]
    (->> ss
         parser
         first
         ;reverse
         ;flatten
         ;(map to-int)
         ;(apply +)
         ))

  )

(deftest aoc-15
  (let [input (read-input "16.txt")
        ;_     (prn input)
        ])

  ;(is (= "011111100101" (first (type-4 "00101010000"))))
  ;(is (= "011111100101" (first (type-4 "110100101111111000101000"))))
  ;(is (= "011111100101" (first (parser "110100101111111000101000"))))
  ;
  ;(is (= ["1010" "00010100"] (reverse (first (parser "00111000000000000110111101000101001010010001001000000000")))))
  ;
  ;(is (= ["0001" "0010" "0011"] (reverse (first (parser "11101110000000001101010000001100100000100011000001100000")))))

  ;(parser "101010000000000000101111010001111000")

  (is (= 3 (do-calc "C200B40A82")))
  (is (= 54 (do-calc "04005AC33890")))
  (is (= 7 (do-calc "880086C3E88112")))
  (is (= 1 (do-calc "9C0141080250320F1802104A08")))
  ;
  (is (= 9 (do-calc "CE00C43D881120")))
  (is (= 1 (do-calc "D8005AC2A8F0")))   ;D8005AC2A8F0 produces 1, because 5 is less than 15.
  (is (= 0 (do-calc "F600BC2D8F")))
  (is (= 0 (do-calc "9C005AC2F8F0")))

  ;F600BC2D8F produces 0, because 5 is not greater than 15.
  ;9C005AC2F8F0 produces 0, because 5 is not equal to 15.

  (is (= 2056021084691 (do-calc "A20D5CECBD6C061006E7801224AF251AEA06D2319904921880113A931A1402A9D83D43C9FFCC1E56FF29890E00C42984337BF22C502008C26982801009426937320124E602BC01192F4A74FD7B70692F4A74FD7B700403170400F7002DC00E7003C400B0023700082C601DF8C00D30038005AA0013F40044E7002D400D10030C008000574000AB958B4B8011074C0249769913893469A72200B42673F26A005567FCC13FE673004F003341006615421830200F4608E7142629294F92861A840118F1184C0129637C007C24B19AA2C96335400013B0C0198F716213180370AE39C7620043E0D4788B440232CB34D80260008645C86D16C401B85D0BA2D18025A00ACE7F275324137FD73428200ECDFBEFF2BDCDA70D5FE5339D95B3B6C98C1DA006772F9DC9025B057331BF7D8B65108018092599C669B4B201356763475D00480010E89709E090002130CA28C62300265C188034BA007CA58EA6FB4CDA12799FD8098021400F94A6F95E3ECC73A77359A4EFCB09CEF799A35280433D1BCCA666D5EFD6A5A389542A7DCCC010958D85EC0119EED04A73F69703669466A048C01E14FFEFD229ADD052466ED37BD8B4E1D10074B3FF8CF2BBE0094D56D7E38CADA0FA80123C8F75F9C764D29DA814E4693C4854C0118AD3C0A60144E364D944D02C99F4F82100607600AC8F6365C91EC6CBB3A072C404011CE8025221D2A0337158200C97001F6978A1CE4FFBE7C4A5050402E9ECEE709D3FE7296A894F4C6A75467EB8959F4C013815C00FACEF38A7297F42AD2600B7006A0200EC538D51500010B88919624CE694C0027B91951125AFF7B9B1682040253D006E8000844138F105C0010D84D1D2304B213007213900D95B73FE914CC9FCBFA9EEA81802FA0094A34CA3649F019800B48890C2382002E727DF7293C1B900A160008642B87312C0010F8DB08610080331720FC580")))


  )


01100010000000001000000000000000000101100001000101010110001011001000100000000010000100011000111000110100
;v 3

000 000 0 000000000010110
  000100010101011000101   1001000100000000010000100011000111000110100
