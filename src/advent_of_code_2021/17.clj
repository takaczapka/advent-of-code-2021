(ns advent-of-code-2021.17
  (:require [clojure.test :refer :all]))

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
                              (not (or (< y y1a) (> x x2a)))))
                          (steps start))
        maybe (last ss)
        [xh yh] (first maybe)]
    (when (and (<= x1a xh) (<= xh x2a) (<= y1a yh) (<= yh y2a))
      (apply max (map second maybe)))))

(defn good-velocities [start [x1a x2a] [y1a y2a]]
  (let [ss    (take-while (fn [sss]
                            (let [[x y] (first sss)]
                              (not (or (< y y1a) (> x x2a)))))
                          (steps start))
        maybe (last ss)]
    (when (some? (first (filter (fn [[xh yh]] (and (<= x1a xh) (<= xh x2a) (<= y1a yh) (<= yh y2a))) maybe)))
      start)))

(defn generate-starts [[_ xmax] ymin]
  (for [x (range 1 (inc xmax))
        y (range ymin (inc xmax))]
    [x y]))

(deftest aoc-15
  (is (= [13 3] (next-step (list [7 2] [0 0]))))

  (let [xt [20 30]
        yt [-10 -5]]
    (is (= 3 (max-y [7 2] xt yt)))
    (is (= 0 (max-y [6 0] xt yt)))
    (is (= nil (max-y [5 7] xt yt)))
    (is (= 0 (max-y [9 0] xt yt)))
    (is (= 6 (max-y [6 3] xt yt)))
    (is (= 45 (max-y [6 9] xt yt)))
    (is (= nil (max-y [17 -4] xt yt))))

  (let [xt [20 30]
        yt [-10 -5]]

    (let [rr (->> (map #(good-velocities % xt yt) (generate-starts xt (first yt)))
                  (remove nil?))]
      (is (= 112 (count rr)))))

  (let [xt [217 240]
        yt [-126 -69]]
    (let [rr (->> (map #(good-velocities % xt yt) (generate-starts xt (first yt)))
                  (remove nil?))]
      (is (= 2321 (count rr)))))
  )
