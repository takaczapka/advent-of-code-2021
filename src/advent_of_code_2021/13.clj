(ns advent-of-code-2021.13
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn rotate [vs]
  (apply map vector vs))

(defn make-board [vs]
  (let [x (inc (apply max (map first vs)))
        y (inc (apply max (map second vs)))
        b (vec (repeat y (vec (repeat x \.))))]

    (reduce (fn [acc v]
              (assoc-in acc (reverse v) \#))
            b vs)))

(defn make-moves [b]
  (let [aa (map (fn [s] (nth (str/split s #" ") 2)) (str/split b #"\n"))
        bb (map (fn [s] (let [[x y] (str/split s #"=")]
                          [(keyword x) (Integer/parseInt y)])) aa)]
    bb))

(defn read-normalise-input [file]
  (let [[a b] (->
                (io/resource file)
                (slurp)
                (str/split #"\n\n"))
        vs    (vec (map (fn [s] (vec (map #(Integer/parseInt %) (str/split s #",")))) (str/split a #"\n")))
        board (make-board vs)
        moves (make-moves b)]
    [board moves]))

(defn merge-me [r1 r2]
  (map (fn [a b]
         (if (or (= a \#) (= b \#))
           \#
           \.)
         )
       r1
       r2))

(defn fold-y [board]
  (let [ys (quot (count board) 2)
        [p1 p2] (split-at ys board)]
    (map (fn [a b]
           (merge-me a b))
         p1 (reverse p2))))

(defn fold-x [board]
  (rotate (fold-y (rotate board))))

(defn count-dots [b]
  (count (filter #(= \# %) (flatten b))))

(defn aoc-13-1 [board]
  (count-dots (fold-x board)))

(defn aoc-13-2 [board moves]
  (reduce (fn [acc [k _]]
            (if (= :x k)
              (fold-x acc)
              (fold-y acc)))
          board
          moves))

(deftest aoc-13
  (let [[board moves] (read-normalise-input "13.txt")]

    (is (= 747 (aoc-13-1 board)))

    (is (= '(".##..###..#..#.####.###...##..#..#.#..#."
              "#..#.#..#.#..#....#.#..#.#..#.#..#.#..#."
              "#..#.#..#.####...#..#..#.#....#..#.####."
              "####.###..#..#..#...###..#....#..#.#..#."
              "#..#.#.#..#..#.#....#....#..#.#..#.#..#."
              "#..#.#..#.#..#.####.#.....##...##..#..#.")
           (map #(str/join %) (aoc-13-2 board moves))))))


