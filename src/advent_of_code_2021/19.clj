(ns advent-of-code-2021.19
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [clojure.zip :as zip]))

(defn read-input [file]
  (let [ss (-> file
               io/resource
               slurp
               (str/split #"\n\n"))
        aa (map (fn [s]
                  (map (fn [ss] (mapv #(Integer/parseInt %) (str/split ss #",")))
                       (rest (str/split s #"\n")))) ss)
        ]
    aa
    )
  )

(defn abs [n] (max n (- n)))

(defn all-pairs [n]
  (remove nil? (for [x (range 0 n)
                     y (range x n)]
                 (when (not= x y) [x y]))))

(defn dbl [x] (* x x))

(defn dist [[x1 y1 z1] [x2 y2 z2]]
  (Math/sqrt (+ (dbl (- x1 x2)) (dbl (- y1 y2)) (dbl (- z1 z2))))
  )

(defn calc-dist [svs]
  (let [vs (vec svs)]
    (mapv (fn [[x y]] (dist (get vs x) (get vs y))) (all-pairs (count vs)))))

(defn transl [s [x y]]
  (set (map (fn [[a b]] [(+ a x) (+ b y)]) s)))

(deftest aoc-19

  ;(let [s-1   (set [[0, 2] [4, 1] [3, 3] [5 1]])
  ;      s-2   (set [[-1, -1] [-5, 0] [-2, 1] [7 8]])
  ;
  ;      ;s-1-d (calc-dist s-1)
  ;      ;s-2-d (calc-dist s-2)
  ;
  ;      ;_ (prn :s-ii (set/intersection s-1-d s-2-d))
  ;
  ;      sc-tx [5 2]]
  ;  )

  (let [ss (read-input "19-1.txt")
        dd (mapv calc-dist ss)]
    (prn :firstdd (first dd))
    (prn :firstccc (count (first dd)))
    ;(prn :alldd (apply set/intersection dd))
    ;(prn :allddccc (count (apply set/intersection dd)))

    ;(prn :size (count ss))
    )


  )