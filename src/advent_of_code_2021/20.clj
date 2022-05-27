(ns advent-of-code-2021.20
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



(defn px [alg img [x y] default]
  (let [a   (get-in img [(dec y) (dec x)] default)
        b   (get-in img [(dec y) x] default)
        c   (get-in img [(dec y) (inc x)] default)
        d   (get-in img [y (dec x)] default)
        e   (get-in img [y x] default)
        f   (get-in img [y (inc x)] default)
        g   (get-in img [(inc y) (dec x)] default)
        h   (get-in img [(inc y) x] default)
        i   (get-in img [(inc y) (inc x)] default)
        r   (str/join (map #(if (= % \.) \0 \1) (vector a b c d e f g h i)))
        res (Integer/parseInt r 2)
        ;_ (prn :r r)
        ;_ (prn :res res)
        ]
    (get alg res)))

(defn translate [alg img default]
  (prn :def default)
  (let [res
        (mapv (fn [y]
                (mapv (fn [x]
                        (px alg img [x y] default))
                      (range -1 (+ (count (first img)) 1))))
              (range -1 (+ (count img) 1)))]
    res
    )

  )

(defn steps [a i n]
  (->>
    (iterate (fn [[img d]]
               (if (= d \.)
                 [(translate a img \#) \# ]
                 [(translate a img \.) \. ]))
             [i \#])
    (drop n)
    first
    first))

(deftest aoc-20
  (let [[a i] (read-input "20.txt")]
    ; 5425

    (->>
      ;(translate a (translate a i \.) \#)
      (steps a i 50)

      flatten
      (filter #(= % \#))
      count
      pprint
      )
    )

  )
