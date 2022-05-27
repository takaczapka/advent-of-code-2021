(ns advent-of-code-2021.9
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input [file]
  (vec (map #(vec (map (fn [c] (- (int c) 48)) (vec %)))
            (->
              (io/resource file)
              (slurp)
              (str/split #"\n")))))

(defn is-low [i x y]
  (let [p     (get-in i [y x])
        up    (get-in i [(dec y) x] (inc p))
        down  (get-in i [(inc y) x] (inc p))
        left  (get-in i [y (dec x)] (inc p))
        right (get-in i [y (inc x)] (inc p))]
    (and
      (< p up)
      (< p down)
      (< p left)
      (< p right)
      )))

(defn find-low [i]
  (for [x (range 0 (count (first i)))
        y (range 0 (count i))
        :let [p [x y]]
        :when (is-low i x y)]
    p))


(defn is-good [i x y]
  (let [p (get-in i [y x])]
    (and (some? p) (not= p 9))))

(defn next-gen [i xs]
  (let [n (mapcat (fn [[x y]]
                 [[x (dec y)] [x (inc y)] [(dec x) y] [(inc x) y]]) xs)
        new (filter
              (fn [[x y]] (is-good i x y))
              (set/difference (set n) (set xs) ))
        ]
    (if (< 0 (count new))
      (next-gen i (set/union xs new))
      xs)))

(deftest aoc-9
  ;(let [i (read-input "9.txt")
  ;      low (find-low i)]
  ;  (prn (apply + (map inc low)))
  ;  )

  (let [i   (read-input "9.txt")
        low (find-low i)]
    (prn
      (apply *
             (->> (map (fn [[x y]] (next-gen i [[x y]] )) low)
                  (filter #(< 0 (count %)))
                  (map count)
                  sort
                  reverse
                  (take 3)))
      )

    )

  )
