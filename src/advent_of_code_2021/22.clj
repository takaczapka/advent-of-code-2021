(ns advent-of-code-2021.22
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn read-normalise-input [file]
  (let [ls (->
             (io/resource file)
             (slurp)
             (str/split #"\n"))]
    (map
      (fn [s]
        (let [[sig coords] (str/split s #"\ ")
              [x y z] (mapv
                        (fn [cc] (mapv
                                   #(Integer/parseInt %)
                                   (str/split (subs cc 2) #"\.\.")))
                        (str/split coords #","))]
          {:sig (if (= sig "on") true false) :x x :y y :z z}))
      ls)))

(defn is-in [[x y z] [x1 x2] [y1 y2] [z1 z2]]
  (and (>= x x1) (<= x x2)
       (>= y y1) (<= y y2)
       (>= z z1) (<= z z2)))

(defn is-on [xyz steps]
  (let [a (first (filter (fn [{:keys [x y z]}] (is-in xyz x y z)) (reverse steps)))]
    (and (some? a) (:sig a)))
  )

;;;; part 1
(defn intersection [[x1 x2] [x3 x4]]
  (if (some? x1)
    [(min x1 x3) (max x2 x4)]
    [x3 x4]))

(defn is-contained [[x1 x2] [x3 x4]]
  (and (>= x1 x3) (<= x2 x4)))

(defn aasd [[x1 x2] [x3 x4]]
  (cond
    (sorted? [x1 x2 x3 x4]) [x1 x2]
    (sorted? [x1 x3 x2 x4]) [x1 x3]
    (sorted? [x1 x3 x4 x2]) [x1 x2]
    (sorted? [x3 x1 x4 x2]) [x4 x2]
    (sorted? [x3 x4 x1 x2]) [x1 x2]
    (sorted? [x3 x1 x2 x4]) [0 0]))

(defn do-magic [[x1 y1 z1] [x2 y2 z2]]
  (cond
    (and (is-contained y1 y2) (is-contained z1 z2)) [(aasd x1 x2) y1 z1]
    (and (is-contained x1 x2) (is-contained z1 z2)) [x1 (aasd y1 y2) z1]
    (and (is-contained x1 x2) (is-contained y1 y2)) [x1 y1 (aasd z1 z2)]
    :else [x1 y1 z1]))

(defn find-boundaries [ss]
  (reduce (fn [[x' y' z'] {:keys [sig x y z]}]
            (if sig
              [(intersection x' x) (intersection y' y) (intersection z' z)]
              (do-magic [x' y' z'] [x y z])))
          []
          ss))

(defn aoc-22-1 [steps]
  (let [[[x1 x2] [y1 y2] [z1 z2]] (find-boundaries steps)
        res (for [x (range (dec x1) (inc x2))
                  y (range (dec y1) (inc y2))
                  z (range (dec z1) (inc z2))]
              (is-on [x y z] steps))]
    (count (filter true? res))))

; part 2
(defn isect? [[a-x1 a-x2] [b-x1 b-x2]]
  (or
    (and (>= a-x1 b-x1) (<= a-x1 b-x2))
    (and (>= a-x2 b-x1) (<= a-x2 b-x2))
    (and (<= a-x1 b-x1) (>= a-x2 b-x2))))

(defn intersects? [c1 c2]
  (every? true? (map isect? c1 c2)))

(defn is-valid [[[x1 x2] [y1 y2] [z1 z2]]]
  (and (<= x1 x2) (<= y1 y2) (<= z1 z2)))

(defn filter-valid [vs]
  (filter is-valid vs))

(defn calc-inters [[a-x1 a-x2] [b-x1 b-x2]]
  [[b-x1 (dec a-x1)] [(inc a-x2) b-x2]])

(defn calc-common [[a-x1 a-x2] [b-x1 b-x2]]
  [(if (<= a-x1 b-x1) b-x1 a-x1)
   (if (<= b-x2 a-x2) b-x2 a-x2)])

(defn diff-x [c1 c2]
  (if (intersects? c1 c2)
    (let [[ax bx] (calc-inters (nth c1 0) (nth c2 0))]
      [[ax (nth c2 1) (nth c2 2)]
       [bx (nth c2 1) (nth c2 2)]])
    [c2]))

(defn diff-y [c1 c2]
  (if (intersects? c1 c2)
    (let [[ay by] (calc-inters (nth c1 1) (nth c2 1))
          xx (calc-common (nth c1 0) (nth c2 0))]
      [[xx ay (nth c2 2)]
       [xx by (nth c2 2)]])
    [c2]))

(defn diff-z [c1 c2]
  (if (intersects? c1 c2)
    (let [[az bz] (calc-inters (nth c1 2) (nth c2 2))
          xx (calc-common (nth c1 0) (nth c2 0))
          yy (calc-common (nth c1 1) (nth c2 1))]
      [[xx yy az]
       [xx yy bz]])
    [c2]))

(defn diff [c1 c2]
  (filter-valid (concat
                  (diff-x c1 c2)
                  (diff-y c1 c2)
                  (diff-z c1 c2))))

(defn diff-all [cs c]
  (mapcat #(diff c %) cs))

(defn find-diffs [cs new-addition]
  (set (reduce (fn [acc n]
                 (set (diff-all acc n)))
               [new-addition]
               cs)))

(defn calc-area [[[x1 x2] [y1 y2] [z1 z2]]]
  (* (inc (- x2 x1)) (inc (- y2 y1)) (inc (- z2 z1))))

(defn calc-areas [cs]
  (apply + (map calc-area cs)))

(defn aoc-22-2 [is]
  (let [input             (reverse is)
        sig               (:sig (first input))
        initial-area-size (if sig
                            (calc-areas [[(:x (first input)) (:y (first input)) (:z (first input))]])
                            0)]

    (:area-size
      (reduce (fn [{:keys [area-size unique-blocks-turned-on]} {:keys [sig x y z]}]
                (let [
                      new-blocks         (find-diffs unique-blocks-turned-on [x y z])
                      new-additions-size (when sig (calc-areas new-blocks))]
                  {:area-size               (if new-additions-size
                                              (+ new-additions-size area-size)
                                              area-size)
                   :unique-blocks-turned-on (concat unique-blocks-turned-on new-blocks)})
                )
              {:area-size               initial-area-size
               :unique-blocks-turned-on [[(:x (first input)) (:y (first input)) (:z (first input))]]}
              (rest input)))))

(deftest aoc-21-2

  (is (= [[[0 4] [0 10] [0 10]] [[16 20] [0 10] [0 10]]]
         (diff [[5 15] [0 10] [0 10]] [[0 20] [0 10] [0 10]])))
  (is (= (list)
         (diff [[0 20] [0 10] [0 10]] [[5 15] [0 10] [0 10]])))

  (is (= [[[0 10] [0 4] [0 10]] [[0 10] [16 20] [0 10]]]
         (diff [[0 10] [5 15] [0 10]] [[0 10] [0 20] [0 10]])))

  (is (= [[[0 10] [0 10] [0 4]] [[0 10] [0 10] [16 20]]]
         (diff [[0 10] [0 10] [5 15]] [[0 10] [0 10] [0 20]])))
  (is (intersects? [[0 10] [0 10] [0 10]] [[0 10] [9 25] [9 25]]))
  (is (intersects? [[0 10] [0 10] [0 10]] [[0 10] [10 25] [9 25]]))
  (is (intersects? [[0 10] [0 10] [0 10]] [[10 11] [10 25] [10 25]]))
  (is (false? (intersects? [[0 10] [0 10] [0 10]] [[0 10] [20 25] [20 25]])))
  (is (false? (intersects? [[0 10] [20 25] [20 25]] [[0 10] [0 10] [0 10]])))
  (is (false? (intersects? [[0 10] [0 10] [0 10]] [[10 10] [20 25] [20 25]])))
  (is (false? (intersects? [[0 10] [0 10] [0 10]] [[10 10] [20 25] [20 25]])))
  (is (false? (intersects? [[0 10] [0 10] [0 10]] [[20 21] [20 21] [20 21]])))

  ; part 1
  (is (= 39 (aoc-22-1 (read-normalise-input "22-1.txt"))))
  (is (= 590784 (aoc-22-1 (read-normalise-input "22-2.txt"))))
  (is (= 606484 (aoc-22-1 (read-normalise-input "22-3.txt"))))

  ; part 2
  (is (= 39
         (aoc-22-2 (read-normalise-input "22-1.txt"))))
  (is (= 590784)
      (aoc-22-2 (read-normalise-input "22-2.txt")))
  (is (= 606484
         (aoc-22-2 (read-normalise-input "22-3.txt"))))

  (is (= 2758514936282235
         (aoc-22-2 (read-normalise-input "22-5.txt"))))

  (is (= 1162571910364852
         (aoc-22-2 (read-normalise-input "22-4.txt")))))

