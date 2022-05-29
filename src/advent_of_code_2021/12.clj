(ns advent-of-code-2021.12
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-normalise-input [file]
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

(defn not-small-cave-twice? [path x]
  (let [freqs (map first (filter #(and (< 1 (second %)) (= (first %) (str/lower-case (first %)))) (frequencies path)))]
    (or (= 0 (count freqs))
        (not (.contains path x)))))

(defn is-valid-move [path x]
  (or (= x "end")
      (not
        (= x (str/lower-case x)))
      (not-small-cave-twice? path x)))

(defn next-move-for-path [m path]
  (if (= (first path) "end")
    [path]
    (let [[x _] path
          next-moves  (get m x)
          valid-moves (filter #(is-valid-move path %) next-moves)]
      (map #(cons % path) valid-moves))))

(defn next-moves-in-paths [m paths]
  (mapcat
    #(next-move-for-path m %)
    paths))

(defn find-the-end [i current-paths]
  (let [new-paths (next-moves-in-paths i current-paths)]
    (if (= current-paths new-paths)
      current-paths
      (find-the-end i new-paths))))

(deftest aoc-12
  (is (= 96988 (count
                 (find-the-end
                   (read-normalise-input "12.txt")
                   ['("start")])))))