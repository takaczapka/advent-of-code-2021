(ns advent-of-code-2021.21
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [clojure.zip :as zip]))

(defn read-input [file]
  (let [[alg img]
        (->
          (io/resource file)
          (slurp)
          (str/split #"\n\n"))]
    [(str/join (str/split alg #"\n"))
     (mapv vec (str/split img #"\n"))]))

(def rolls
  (->>
    (iterate inc 1)
    (partition 3)
    (map #(apply + %))))

(defn step [c r]
  (inc (mod (+ (dec c) r) 10)))

(defn next-move [{:keys [pos score]} roll]
  {:pos (step pos roll) :score (+ score (step pos roll))})

(defn play-steps [p1-start p2-start]
  (reductions (fn [game [roll p]]
                (assoc game p (next-move (get game p) roll)))
              [{:pos p1-start :score 0} {:pos p2-start :score 0}]
              (map vector rolls (cycle [0 1]))))

(defn take-while+
  [pred coll]
  (lazy-seq
    (when-let [[f & r] (seq coll)]
      (if (pred f)
        (cons f (take-while+ pred r))
        [f]))))

(defn rolls-to-win [rs]
  (* 3 (dec (count rs))))

(defn score-of-losing-player [rs max-score]
  (let [[p1 p2] (last rs)]
    (if (<= max-score (:score p1))
      (:score p2)
      (:score p1))))

(defn boom [p1-start p2-start max-score]
  (let [res
        (->> (play-steps p1-start p2-start)
             (take-while+ (fn [[p1 p2]]
                            (and (> max-score (:score p1))
                                 (> max-score (:score p2))))))]
    (* (rolls-to-win res) (score-of-losing-player res max-score)))
  )

(defn is-finished [{:keys [max-score players]}]
  (let [[player-1 player-2] players]
    (or (<= max-score (:score player-1)) (<= max-score (:score player-2)))))

(defn score [{:keys [players boost]}]
  (let [[player-1 player-2] players]
    (if (> (:score player-1) (:score player-2)) [1 0] [0 1])))

(def next-moves
  {3 1N
   4 3N
   5 6N
   6 7N
   7 6N
   8 3N
   9 1N})

(defn next-movex [{:keys [pos score]} roll]
  {:pos (step pos roll) :score (+ score (step pos roll))})

(defn next-game [{:keys [next-player players boost depth] :as game} step]
  (let [player  (get players next-player)
        player' (next-movex player step)]
    (merge game {:next-player (mod (inc next-player) 2) :players (assoc players next-player player')
                 ;:boost       (if (= boost 0) n-boost (* boost n-boost))
                 ;:depth (inc depth)
                 })))

(defn calc-score-so-far [vs]
  (apply map + vs))

(defn times-boost [rs boost]
  (map (fn [[a b]] [(* boost a) (* boost b)]) rs))

(def m-rplay (memoize rplay))

(defn rplay [game]
  (if (is-finished game)
    (score game)
    (calc-score-so-far (map (fn [[step boost]] (map #(* % boost) (m-rplay (next-game game step)))) next-moves))))

(def m-rplay (memoize rplay))

(deftest aoc-21-2
  (let [p1-start 4
        p2-start 10]
    (prn (rplay {:max-score   21
                 :depth       0
                 :next-player 0
                 :boost       1
                 :players     [{:pos p1-start :score 0} {:pos p2-start :score 0}]}))))

; (127019 152976)
; (392 281)

;341960390180808
;(* 3 3151502992942)
;3045395899116


;(deftest aoc-21
;  (is (= 855624 (boom 4 10 1000)))                               ; p2 wins
;  (is (= 739785 (boom 4 8 1000)))                                ; p1 wins
;)
