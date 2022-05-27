(ns advent-of-code-2021.11
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

(defn update-cell [i x y]
  (let [v (get-in i [y x])]
    (if (and (some? v) (number? v))
      (assoc-in i [y x] (inc v))
      i)))

(defn neigh [x y]

  [[(inc x) (inc y)]
   [x (inc y)]
   [(dec x) (inc y)]
   [(inc x) y]
   [(inc x) (dec y)]
   [x (dec y)]
   [(dec x) (dec y)]
   [(dec x) y]])


(defn burst [i bs]
  (reduce (fn [i' b]
            (update-cell i' (first b) (second b)))
          i
          (mapcat #(neigh (first %) (second %)) bs)))

(defn to-vec [l]
  (vec (map vec l)))

(defn init [i]
  (to-vec (map #(map inc %) i)))

(defn one-go [i]
  (let [
        b  (for [x (range 0 (count (first i)))
                 y (range 0 (count i))
                 :let [flashing [x y]
                       v        (get-in i [y x])]
                 :when (and (number? v) (< 9 v))]
             flashing)
        i-x (reduce (fn [acc bb]
                      (assoc-in acc (reverse bb) :x)
                      ) i b)]
    (if (empty? b)
      i-x
      (one-go (burst i-x b))
      )))

(defn generation [i]
  (let [r (one-go (init i))]
    [
     (map (fn [ls] (map #(if (= % :x) 0 %) ls)) r)
     (count (filter #(= :x %) (flatten r)))])
  )


(deftest aoc-11
  (let [i (read-input "11.txt")
        calc (iterate (fn [a] (generation (first a))) [i 0])
        res (take 101 calc)]
    (prn (apply + (map second res )))

    (prn (first (filter #(= (second (second %)) 100) (map-indexed (fn [idx c] [idx c]) calc))))

    ;(prn (one-go ))
    )

  )