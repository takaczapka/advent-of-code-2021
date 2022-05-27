(ns advent-of-code-2021.12
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input [file]
  (let [a (vec (map #(str/split % #"-")
                    (->
                      (io/resource file)
                      (slurp)
                      (str/split #"\n"))))]


    (->> (mapcat (fn [[k v]] (cond
                               (or (= k "start") (= v "end")) [[k v]]
                               (= v "start") [[v k]]
                               :else [[k v] [v k]])) a)
         (group-by first)
         (map (fn [[k v]] [k (map second v)]))
         (into {}))))



(defn is-valid-move [path x]
  (or (= x "end")
      (not (and
             (.contains path x)
             (= x (str/lower-case x))))))


(defn not-small-cave-twice? [path x]
  (let [freqs (map first (filter #(and (< 1 (second %)) (= (first %) (str/lower-case (first %)))) (frequencies path)))]
    (or (= 0 (count freqs))
        (not (.contains path x))
        ))
  )

(defn is-valid-move-2 [path x]
  (or (= x "end")
      (not
        (= x (str/lower-case x)))
      (not-small-cave-twice? path x)))

(defn next-p [m path]
  (if (= (first path) "end")
    [path]
    (let [[x xs] path
          next-moves  (get m x)
          valid-moves (filter #(is-valid-move-2 path %) next-moves)
          ;_ (prn :next-moves next-moves)
          ;_ (prn :valid-moves valid-moves)
          ]
      (map #(cons % path) valid-moves))))

(defn next-paths [m paths]
  (mapcat
    #(next-p m %)
    paths))

(defn to-the-end [i ps]
  (let [nn (next-paths i ps)]
    ;(prn :ps ps)
    ;(prn :nn nn)
    (if (= ps nn)
      ps
      (to-the-end i nn)
      )))


(deftest aoc-12
  (let [i (read-input "12.txt")
        _ (prn i)
        ;_ (prn :AAA (next-paths i [["start"]]))
        ;_ (prn :BBB (to-the-end i [["start"]]))
        ;_ (prn :BBB (to-the-end i ['("start")]))
        _ (prn :BBB (to-the-end i ['("b" "A" "b" "A" "start")]))
        _ (prn :BBB (count (to-the-end i ['("start")])))
        ]
    )

  )