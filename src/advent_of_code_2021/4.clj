(ns advent-of-code-2021.4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.set :as set]))

(defn read-input [file]
  (->
    (io/resource file)
    (slurp)
    (str/split #"\n")))

(defn rotate [vs]
  (apply map vector vs))

(defn find-solved [bingos]
  (let [winning [:x :x :x :x :x]]
    (first (filter (fn [b] (or (.contains b winning) (.contains (rotate b) winning))) bingos))))

(defn find-all-solved [bingos]
  (let [winning [:x :x :x :x :x]]
    (filter (fn [b] (or (.contains b winning) (.contains (rotate b) winning))) bingos)))

(defn apply-to-bingos [bingos move]
  (map (fn [b] (map (fn [br]
                      (replace {move :x} br)) b)) bingos))

(defn aoc4 [steps bingos]
  (reduce (fn [acc ss]

            (let [new-bingos (apply-to-bingos acc ss)
                  winer      (find-solved new-bingos)]
              (if (some? winer)
                (reduced [winer ss])
                new-bingos)
              )

            )
          bingos steps)
  )

(defn aoc4-2 [steps bingos]
  (let [{:keys [winners]}
        (reduce (fn [{:keys [winners not-yet]} ss]
                  (let [new-bingos  (apply-to-bingos not-yet ss)
                        new-winners (find-all-solved new-bingos)
                        _ (prn ss)]
                    {:winners (if (empty? new-winners) winners (concat winners [[ss new-winners]])) :not-yet (remove #(.contains new-winners %) new-bingos)}
                    ))
                {:winners [] :not-yet bingos} steps)]
    (last winners)
    )

  )


(deftest aoc4-test
  (let [input       (read-input "4.txt")
        bingo-input (str/split (first input) #",")
        bingos      (->> (rest input)
                         (partition 6)
                         (map rest)
                         (map (fn [m] (map (fn [s] (remove #(= "" %) (str/split s #"\s+"))) m))))
        [winner last-number]      (aoc4 bingo-input bingos)
        [ll-n [last-winner]] (aoc4-2 bingo-input bingos)
        _ (prn ll-n)
        ]

    (prn (->> winner
              flatten
              (remove #(= :x %))
              (map #(Integer/parseInt %))
              (apply +)
              (* (Integer/parseInt last-number))))

    (prn last-winner)
    (prn (->> last-winner
              flatten
              (remove #(= :x %))
              (map #(Integer/parseInt %))
              (apply +)
              (* (Integer/parseInt ll-n))))
    (prn last-winner)
    )


  ;(let [b [[["38" "80" "23" "60" "82"] ["25" "35" "28" "47" "39"] ["40" "" "0" "30" "48" "76"] ["32" "41" "49" "69" "" "4"] ["13" "42" "89" "20" "12"]]]]
  ;
  ;  (prn (apply-to-bingos b "38"))
  ;
  ;  (is (= [] (apply-to-bingos b "38")))
  ;
  ;
  ;  )
  )