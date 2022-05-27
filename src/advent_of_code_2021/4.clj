(ns advent-of-code-2021.4
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [advent-of-code-2021.common :as common]))

(defn find-all-solved [bingos]
  (let [winning [:x :x :x :x :x]]
    (filter (fn [b] (or (.contains b winning) (.contains (common/rotate b) winning))) bingos)))

(defn find-solved [bingos]
  (first (find-all-solved bingos)))

(defn apply-to-bingos [bingos move]
  (map
    #(map (fn [br] (replace {move :x} br)) %)
    bingos))

(defn aoc4 [steps bingos]
  (reduce (fn [acc ss]
            (let [new-bingos (apply-to-bingos acc ss)
                  winner     (find-solved new-bingos)]
              (if (some? winner)
                (reduced [winner ss])
                new-bingos)))
          bingos steps))

(defn aoc4-2 [steps bingos]
  (let [{:keys [winners]}
        (reduce (fn [{:keys [winners not-yet]} ss]
                  (let [new-bingos  (apply-to-bingos not-yet ss)
                        new-winners (find-all-solved new-bingos)]
                    {:winners (if (empty? new-winners) winners (concat winners [[ss new-winners]])) :not-yet (remove #(.contains new-winners %) new-bingos)}))
                {:winners [] :not-yet bingos} steps)]
    (last winners)))

(defn calc-score [winner last-number]
  (->> winner
       flatten
       (remove #(= :x %))
       (map #(parse-long %))
       (apply +)
       (* (parse-long last-number))))

(deftest aoc4-test
  (let [input       (common/read-input "4.txt")
        bingo-input (str/split (first input) #",")
        bingos      (->> (rest input)
                         (partition 6)
                         (map rest)
                         (map (fn [m] (map (fn [s] (remove #(= "" %) (str/split s #"\s+"))) m))))
        [winner last-number] (aoc4 bingo-input bingos)
        [ll-n [last-winner]] (aoc4-2 bingo-input bingos)]

    (is (= 87456
           (calc-score winner last-number)))

    (is (= 15561
           (calc-score last-winner ll-n)))))