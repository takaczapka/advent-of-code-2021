(ns advent-of-code-2021.14
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn make-mapping [s]
  (let [ss  (str/split s #"\n")
        sss (map (fn [x] (str/split x #" -> ")) ss)]
    (into {} (map (fn [[a b]] [(vec a) (first b)]) sss))))

(defn read-input [file]
  (let [[a b] (->
                (io/resource file)
                (slurp)
                (str/split #"\n\n"))
        mapping (make-mapping b)]
    [a mapping]))

(defn prepare [p]
  (frequencies (partition 2 1 p)))

(defn next-step [pf m]
  (->> (mapcat (fn [[[a b] n]] [{[a (get m [a b])] n} {[(get m [a b]) b] n}]) pf)
       (apply merge-with +)))

(defn generate [init m num-steps]
  (first (drop num-steps (iterate (fn [p] (next-step p m)) (prepare init)))))

(defn extract-occurrences [pf last-letter]
  (->> (map (fn [[[a _] n]] {a n}) pf)
       (apply merge-with +)
       (merge-with + {last-letter 1})))

(defn calc-res [polymer-template gen-res]
  (let [vs (vals (extract-occurrences gen-res (last polymer-template)))]
    (- (apply max vs) (apply min vs))))

(deftest aoc-14
  (let [[p m] (read-input "14.txt")]
    ; aoc-14-1
    (is (= 2233 (calc-res p (generate p m 10))))
    ; aoc-14-2
    (is (= 2884513602164 (calc-res p (generate p m 40))))))
