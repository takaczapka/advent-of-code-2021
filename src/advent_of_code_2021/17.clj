(ns advent-of-code-2021.17
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(defn next-step [xs]
  (let [[xy xy2] (take 2 xs)
        [x1 y1] xy
        [x2 y2] xy2
        vx (cond
             (< 0 (- x1 x2)) (dec x1)
             (= 0 (- x1 x2)) x1
             :else (inc x1))
        vy (dec (- y1 y2))]
    [(+ (- x1 x2) vx) (+ y1 vy)]))

(defn take-while+
  [pred coll]
  (lazy-seq
    (when-let [[f & r] (seq coll)]
      (if (pred f)
        (cons f (take-while+ pred r))
        [f]))))

(defn steps [start]
  (iterate (fn [xs] (cons (next-step xs) xs)) (list start [0 0])))

(defn max-y [start [x1a x2a] [y1a y2a]]
  (let [ss    (take-while (fn [sss]
                             (let [[x y] (first sss)]
                               (not (or (< y y1a) (> x x2a))))
                             ) (steps start))
        maybe (last ss)
        ;_     (prn :naybe maybe)
        [xh yh] (first maybe)]
    (when (and (<= x1a xh) (<= xh x2a) (<= y1a yh) (<= yh y2a))
      (apply max (map second maybe)))))

(defn good-velo [start [x1a x2a] [y1a y2a]]
  (let [ss    (take-while (fn [sss]
                            (let [[x y] (first sss)]
                              (not (or (< y y1a) (> x x2a))))
                            ) (steps start))
        maybe (last ss)
        ;[xh yh] (first maybe)
        ]
    (when (some? (first (filter (fn [[xh yh]] (and (<= x1a xh) (<= xh x2a) (<= y1a yh) (<= yh y2a))) maybe)))
      start
      )))

(defn generate-starts [[xmin xmax] ymin]
  (for [x (range 1 (inc xmax))
        y (range ymin (inc xmax))]
    [x y]))

(deftest aoc-15

  ;(prn (generate-starts 20))

  ;(prn (next-step (list [7 2] [0 0])))
  (is (= [13 3] (next-step (list [7 2] [0 0]))))



  ;(clojure.pprint/pprint (nth (steps [7 6]) 20))


  ;(clojure.pprint/pprint (nth (steps [6 9]) 20))
  ;(max-y [6 9] [20 30] [-10 -5])



  (let [; x=20..30, y=-10..-5
        xt [20 30]
        yt [-10 -5]
        ]
    (is (= 3 (max-y [7 2] xt yt)))
    (is (= 0 (max-y [6 0] xt yt)))
    (is (= nil (max-y [5 7] xt yt)))
    (is (= 0 (max-y [9 0] xt yt)))
    (is (= 6 (max-y [6 3] xt yt)))
    (is (= 45 (max-y [6 9] xt yt)))
    (is (= nil (max-y [17 -4] xt yt)))

    ;(let [rr (->> (map #(max-y % xt yt) (generate-starts (first xt) (first yt)))
    ;              (remove nil?))
    ;      _  (prn (apply max rr))])

    )

  ;....................TTTTTTTT#TT


  (let [xt [20 30]
        yt [-10 -5]]

    (let [rr (->> (map #(good-velo % xt yt) (generate-starts xt (first yt)))
                  (remove nil?))
          _  (prn :rr rr)
          _  (prn :rr-c (count rr))
          ])

    )

  ;(let [s1 (set [[6 0] [6 1] [6 2] [6 3] [6 4] [6 5] [6 6] [6 7] [6 8] [6 9] [7 -1] [7 0] [7 1] [7 2] [7 3] [7 4] [7 5] [7 6] [7 7] [7 8] [7 9] [8 -2] [8 -1] [8 0] [8 1] [9 -2] [9 -1] [9 0] [10 -2] [10 -1] [11 -4] [11 -3] [11 -2] [11 -1] [12 -4] [12 -3] [12 -2] [13 -4] [13 -3] [13 -2] [14 -4] [14 -3] [14 -2] [15 -4] [15 -3] [15 -2] [20 -10] [20 -9] [20 -8] [20 -7] [20 -6] [20 -5] [21 -10] [21 -9] [21 -8] [21 -7] [21 -6] [21 -5] [22 -10] [22 -9] [22 -8] [22 -7] [22 -6] [22 -5] [23 -10] [23 -9] [23 -8] [23 -7] [23 -6] [23 -5] [24 -10] [24 -9] [24 -8] [24 -7] [24 -6] [24 -5] [25 -10] [25 -9] [25 -8] [25 -7] [25 -6] [25 -5] [26 -10] [26 -9] [26 -8] [26 -7] [26 -6] [26 -5] [27 -10] [27 -9] [27 -8] [27 -7] [27 -6] [27 -5] [28 -10] [28 -9] [28 -8] [28 -7] [28 -6] [28 -5] [29 -10] [29 -9] [29 -8] [29 -7] [29 -6] [29 -5] [30 -10] [30 -9] [30 -8] [30 -7] [30 -6] [30 -5]])
  ;      s2 (set [[23 -10] [25 -9] [27 -5] [29 -6] [22 -6] [21 -7] [9 0] [27 -7] [24 -5]
  ;               [25 -7] [26 -6] [25 -5] [6 8] [11 -2] [20 -5] [29 -10] [6 3] [28 -7]
  ;               [8 0] [30 -6] [29 -8] [20 -10] [6 7] [6 4] [6 1] [14 -4] [21 -6]
  ;               [26 -10] [7 -1] [7 7] [8 -1] [21 -9] [6 2] [20 -7] [30 -10] [14 -3]
  ;               [20 -8] [13 -2] [7 3] [28 -8] [29 -9] [15 -3] [22 -5] [26 -8] [25 -8]
  ;               [25 -6] [15 -4] [9 -2] [15 -2] [12 -2] [28 -9] [12 -3] [24 -6] [23 -7]
  ;               [25 -10] [7 8] [11 -3] [26 -7] [7 1] [23 -9] [6 0] [22 -10] [27 -6]
  ;               [8 1] [22 -8] [13 -4] [7 6] [28 -6] [11 -4] [12 -4] [26 -9] [7 4]
  ;               [24 -10] [23 -8] [30 -8] [7 0] [9 -1] [10 -1] [26 -5] [22 -9] [6 5]
  ;               [7 5] [23 -6] [28 -10] [10 -2] [11 -1] [20 -9] [14 -2] [29 -7] [13 -3]
  ;               [23 -5] [24 -8] [27 -9] [30 -7] [28 -5] [21 -10] [7 9] [6 6] [21 -5]
  ;               [27 -10] [7 2] [30 -9] [21 -8] [22 -7] [24 -9] [20 -6] [6 9] [29 -5]
  ;               [8 -2] [27 -8] [30 -5] [24 -7]])
  ;
  ;      ]
  ;  (prn :s1-s2 (set/difference s1 s2))
  ;  (prn :s2-s1 (set/difference s2 s1))
  ;  )



  (let [; x=217..240 y=-126..-69
        xt [217 240]
        yt [-126 -69]
        ]

    ;(let [rr (->> (map #(max-y % xt yt) (generate-starts (first xt) (first yt)))
    ;              (remove nil?))
    ;      _  (prn (apply max rr))])

    )

  (let [        xt [217 240]
        yt [-126 -69]]

    (let [rr (->> (map #(good-velo % xt yt) (generate-starts xt (first yt)))
                  (remove nil?))
          ;_  (prn :rr rr)
          _  (prn :rr-c (count rr))
          ])

    )
  )
