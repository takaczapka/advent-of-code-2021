(ns advent-of-code-2021.5
  (:require
    [clojure.test :refer :all]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]))

(defn read-input [file]
  (->
    (io/resource file)
    (slurp)
    (str/split #"\n")))

(defn parse-line [l]
  (prn l)
  (let [[a b] (str/split l #" -> ")]
    [(map #(Integer/parseInt %) (str/split a #","))
     (map #(Integer/parseInt %) (str/split b #","))])
  )

(defn parse-input [is]
  (map parse-line is))

(defn sort-points [p1 p2]
  (if (= (first p1) (first p2))
    (if (< (second p1) (second p2))
      [p1 p2]
      [p2 p1])
    (if (< (first p1) (first p2))
      [p1 p2]
      [p2 p1])))

(defn points [p1' p2']
  (let [[p1 p2] (sort-points p1' p2')
        [x y] p1
        [x' y'] p2]
    (cons p2
          (cond
            (= x x') (take (- y' y) (iterate (fn [[a b]] [a (inc b)]) p1))
            (= y y') (take (- x' x) (iterate (fn [[a b]] [(inc a) b]) p1))
            (and (< x x') (< y y')) (take-while #(not= p2 %) (iterate (fn [[a b]] [(inc a) (inc b)]) p1))
            (and (< x x') (> y y')) (take-while #(not= p2 %) (iterate (fn [[a b]] [(inc a) (dec b)]) p1))
            (and (> x x') (< y y')) (take-while #(not= p2 %) (iterate (fn [[a b]] [(dec a) (inc b)]) p1))
            (and (> x x') (> y y')) (take-while #(not= p2 %) (iterate (fn [[a b]] [(dec a) (dec b)]) p1))

            )))
  )

(defn draw [pnts]
  (let [board (vec (repeat 10 (vec (repeat 10 0))))]
    (reduce (fn [bs [a b]] (assoc-in bs [b a] 6))
            board pnts))
  )

(defn count-intersections [cords]
  (count (filter #(< 1 (count %)) (vals (group-by identity (map vec (apply concat (map #(points (first %) (second %)) cords))))))))

(deftest asdasds-test


  (is (= (set [[1 1]]) (set (points [1 1] [1 1]))))

  (is (= (set [[1 1] [1 2]]) (set (points [1 1] [1 2]))))

  (is (= (set [[1 1] [1 2] [1 3] [1 4] [1 5]]) (set (points [1 1] [1 5]))))

  (is (= (set [[2 1] [3 1] [4 1] [5 1]]) (set (points [2 1] [5 1]))))

  (is (= (set [[0 9] [1 9] [2 9] [3 9] [4 9] [5 9]]) (set (points [0,9] [5,9]))))

  (is (= (set [[9,7,] [8,8] [7,9]]) (set (points [9 7] [7 9]))))

  (is (= (set [[1 3] [2 3] [3 3]]) (set (points [1 3] [3 3]))))


  (let [
        input    (str/split "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2" #"\n")
        input (read-input "5.txt")
        _ (prn input)
        cords    (parse-input input)
        _ (prn cords)
        pointszz (map #(points (first %) (second %)) cords)
        ;_        (prn pointszz)
        ;_        (prn (map vec (apply concat pointszz)))
        ]
    (prn (count-intersections cords))
    ;(prn (count (filter #(< 1 (count %)) (vals (group-by identity (map vec (apply concat pointszz)))))))
    ;
    ;(pprint (draw (apply concat pointszz)))
    )

  )