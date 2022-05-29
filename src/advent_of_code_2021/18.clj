(ns advent-of-code-2021.18
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.zip :as zip]))

(defn find-pair-loc [v]
  (first (drop-while #(let [l (zip/node %)]
                        (and (not (and (vector? l) (= 2 (count l)) (int? (first l)) (int? (second l))
                                       (>= (-> % (zip/path) count) 4))) ; ????))
                             (not (zip/end? %))
                             ))
                     (iterate zip/next (zip/vector-zip v)))))

(defn add-to-next-at-right [loc x]
  (let [f-loc (first (drop-while #(and (not (int? (zip/node %))) (not (zip/end? %))) (iterate zip/next loc)))
        y     (zip/node f-loc)]
    (if (and (some? y) (int? y))
      (zip/replace f-loc (+ x y))
      loc)))

(defn add-to-next-at-left [loc x]
  (let [f-loc (first (drop-while
                       #(do
                          (and
                            (not (nil? %))
                            (not (int? (zip/node %)))))
                       (iterate zip/prev (zip/prev loc))))
        y     (when (some? f-loc) (zip/node f-loc))]
    (if (and (some? f-loc) (some? y) (int? y))
      (zip/replace f-loc (+ x y))
      loc)))

(defn find-next-0 [loc]
  (first (drop-while #(let [l (zip/node %)]
                        (and (not (and (int? l) (= 0 l)))
                             (not (zip/end? %))))
                     (iterate zip/next loc))))

(defn explode [v]
  (let [loc (find-pair-loc v)
        [a b] (zip/node loc)]
    (if (and (some? loc) (not (zip/end? loc)))
      (do
        (-> loc
            (zip/replace 0)
            (add-to-next-at-left a)
            (find-next-0)
            (zip/next)
            (add-to-next-at-right b)
            (zip/root)))
      v)))

(defn split [v]
  (let [f-loc (first (drop-while #(and (not (and (int? (zip/node %)) (> (zip/node %) 9))) (not (zip/end? %)))
                          (iterate zip/next (zip/vector-zip v))))
        y     (zip/node f-loc)]
    (if (and (some? y) (int? y))
      (zip/root (zip/replace f-loc [(quot y 2) (quot (inc y) 2)]))
      v)))

(defn shabang [v]
  ;(prn :v v)
  (let [new-v (explode v)]
    (if (= v new-v)
      (let [newer-v (split v)]
        (if (= newer-v v)
          v
          (shabang (split v))))
      (shabang new-v))))

(defn sum-up [vs]
  (reduce (fn [acc v]
            (shabang (vector acc v))) vs))

(defn calc-magnitude [vs]
  (let [[x y] vs]
    (cond
      (and (int? x) (int? y)) (+ (* 3 x) (* 2 y))
      (int? x) (+ (* 3 x) (calc-magnitude y))
      (int? y) (+ (* 3 (calc-magnitude x)) (* 2 y))
      :else (+ (* 3 (calc-magnitude x)) (* 2 (calc-magnitude y)))
      )))

(defn all-combinations [n]
  (let [ff (for [x (range 0 n)
                 y (range (inc x) n)]
             [x y])]
    (vec (concat ff (mapv (comp vec reverse ) ff)))))

(defn the-best-magn [vs]
  (->> (all-combinations (count vs))
       (map (fn [[x y]]
              (calc-magnitude (sum-up [(get vs x) (get vs y)]))))
       (apply max)))

(deftest aoc-18
  ;
  (is (= [[[[0 9] 2] 3] 4] (explode [[[[[9 8] 1] 2] 3] 4])))
  (is (= [7, [6, [5, [7, 0]]]] (explode [7, [6, [5, [4, [3, 2]]]]])))
  (is (= [[6, [5, [7, 0]]], 3] (explode [[6, [5, [4, [3, 2]]]], 1])))
  (is (= [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]] (explode [[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]])))
  (is (= [[3, [2, [8, 0]]], [9, [5, [7, 0]]]] (explode [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]]))) ; this is wrong in spec???
  (is (= [[[[0, 7], 4], [[7, 8], [6, 0]]], [8, 1]] (explode [[[[0, 7], 4], [[7, 8], [6, 0]]], [8, 1]])))
  (is (= [[3, [2, [8, 0]]], [9, [5, [7, 0]]]] (explode [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]])))

  (is (= [[[[0, 7], 4], [[7, 8], [0, [6, 7]]]], [1, 1]] (split [[[[0, 7], 4], [[7, 8], [0, 13]]], [1, 1]])))

  (is (= [[[[0, 7], 4], [[7, 8], [6, 0]]], [8, 1]] (shabang [[[[[4, 3], 4], 4], [7, [[8, 4], 9]]], [1, 1]])))
  (is (= [[[[1, 1], [2, 2]], [3, 3]], [4, 4]])
      (sum-up
        [[1, 1]
         [2, 2]
         [3, 3]
         [4, 4]]))

  (is (=
        [[[[5,0],[7,4]],[5,5]],[6,6]]
        (sum-up
          [[1,1]
           [2,2]
           [3,3]
           [4,4]
           [5,5]
           [6,6]
           ])))

  (is (= [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
         (sum-up
           [
            [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
            [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
            ])))


  (is (=
        [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
        (sum-up
          [
           [[[[4, 0], [5, 4]], [[7, 7], [6, 0]]], [[8, [7, 7]], [[7, 9], [5, 0]]]]
           [[2, [[0, 8], [3, 4]]], [[[6, 7], 1], [7, [1, 6]]]]]
          )
        ))

  (is (=
        [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
        (sum-up
          [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
           [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
           [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
           [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
           [7,[5,[[3,8],[1,4]]]]
           [[2,[2,2]],[8,[8,1]]]
           [2,9]
           [1,[[[9,3],9],[[9,0],[0,7]]]]
           [[[5,[7,4]],7],1]
           [[[[4,2],2],6],[8,7]]
           ])))

  (is (=
        [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
        (sum-up
          [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
           [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
           [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
           [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
           [7,[5,[[3,8],[1,4]]]]
           [[2,[2,2]],[8,[8,1]]]
           [2,9]
           [1,[[[9,3],9],[[9,0],[0,7]]]]
           [[[5,[7,4]],7],1]
           [[[[4,2],2],6],[8,7]]
           ])))


  (is (= 4563 (the-best-magn
                [[[[[9, 5], [9, 4]], [[6, 5], [7, 0]]], 4]
                 [[[5, 2], [[7, 2], 1]], [[[7, 5], [0, 8]], [[6, 9], [7, 3]]]]
                 [[[9, 7], [0, 1]], 9]
                 [1, [[7, 3], [[3, 7], [3, 2]]]]
                 [[9, [[0, 8], 7]], [[3, 1], [[6, 6], [9, 0]]]]
                 [4, [[4, 4], [[7, 7], 1]]]
                 [[[[6, 2], [5, 1]], [[3, 3], 9]], [7, [[5, 7], [5, 0]]]]
                 [[[[4, 8], [4, 9]], [1, [9, 3]]], [1, [1, [6, 1]]]]
                 [[[[4, 7], [3, 4]], [8, 3]], [[3, 7], [0, [1, 8]]]]
                 [[[6, [4, 8]], [4, 5]], [4, [1, 3]]]
                 [[[0, 7], 0], [[6, [1, 8]], [9, [7, 9]]]]
                 [[[[4, 8], [3, 9]], [4, 5]], [1, 1]]
                 [[[4, 2], [0, [6, 7]]], [[[1, 8], 2], [8, 8]]]
                 [[[[1, 1], 7], 5], [[6, [5, 6]], [6, [7, 5]]]]
                 [[[[3, 2], 5], [[5, 3], 1]], [[[0, 4], [9, 6]], 9]]
                 [[6, [7, 6]], 9]
                 [[[[4, 0], [0, 1]], 7], 1]
                 [[[[1, 3], 4], 6], [[1, [4, 2]], [1, 4]]]
                 [[[[6, 9], [4, 1]], [[6, 3], [0, 8]]], [[4, 0], [[3, 2], [2, 9]]]]
                 [[[3, 6], [[2, 0], [3, 2]]], [2, 5]]
                 [[[[4, 3], 5], 5], [[4, [4, 0]], 6]]
                 [[[[4, 0], 3], [[3, 5], 8]], [[8, [4, 4]], [[9, 9], [4, 1]]]]
                 [[[2, 7], 6], 1]
                 [[[[5, 3], [8, 4]], [0, 0]], 4]
                 [[[0, [8, 1]], 0], 3]
                 [[[6, 5], [8, 2]], [[[6, 9], [6, 1]], [9, 9]]]
                 [0, [[4, 9], 6]]
                 [[9, [[9, 9], 4]], [[[4, 7], 1], 2]]
                 [[8, 0], [[[0, 7], 6], [[6, 4], 2]]]
                 [[1, [[2, 4], 8]], 1]
                 [[[[1, 3], 4], [[1, 3], 0]], [[[1, 2], 3], 2]]
                 [[[[2, 1], 2], [5, [2, 8]]], [2, [[6, 0], 2]]]
                 [[[8, [1, 0]], [[6, 7], [9, 6]]], [[2, [9, 7]], 5]]
                 [[[3, [2, 0]], [[3, 2], [0, 0]]], [[[4, 6], [9, 4]], [[7, 8], [5, 1]]]]
                 [[3, [[9, 9], [7, 2]]], [[1, 3], [2, [3, 2]]]]
                 [4, [4, [[9, 5], 6]]]
                 [[[[5, 7], 7], [[3, 4], 0]], [[9, [8, 2]], [2, 3]]]
                 [[[[2, 1], [5, 7]], 4], [[[6, 3], 8], [[1, 6], [5, 1]]]]
                 [[[4, 4], [[0, 9], [7, 8]]], [[2, [2, 5]], 5]]
                 [1, [5, [[3, 7], [8, 2]]]]
                 [[[[9, 5], [8, 6]], [5, 5]], [[[9, 2], 8], [[9, 3], [3, 8]]]]
                 [0, [[9, 5], [[3, 7], 7]]]
                 [[[8, [0, 4]], [[2, 9], 6]], [[6, [8, 0]], 4]]
                 [[0, [3, 5]], [[5, [0, 1]], [[3, 6], 7]]]
                 [[2, [7, 1]], [[[5, 0], [7, 7]], [[2, 3], 9]]]
                 [[5, [9, [3, 9]]], [[8, [3, 7]], [[7, 6], [3, 0]]]]
                 [[[4, [2, 5]], 5], [3, 1]]
                 [[[[4, 3], 1], [[5, 7], 6]], [0, [3, 1]]]
                 [[8, 9], [[[0, 7], 5], [6, [5, 7]]]]
                 [[6, 8], [[5, 8], [[8, 2], [6, 0]]]]
                 [[1, [5, 6]], 5]
                 [[[6, 1], [9, [1, 2]]], 1]
                 [[5, [7, [4, 8]]], [[4, [2, 9]], 5]]
                 [[[2, 2], [[7, 1], 3]], [[[9, 7], [4, 6]], [1, [0, 1]]]]
                 [[3, [6, [4, 5]]], 2]
                 [[[0, 2], [[8, 1], [0, 6]]], [[7, [9, 6]], 0]]
                 [[[[1, 0], [5, 1]], [[0, 6], 5]], [[[1, 8], 8], [[0, 2], 5]]]
                 [[6, [[3, 6], 6]], [[[9, 7], [6, 4]], [[9, 5], 1]]]
                 [[[0, [5, 6]], [9, 0]], [[2, 9], 9]]
                 [1, [[4, [9, 3]], 0]]
                 [[1, 0], [[1, 9], [4, 8]]]
                 [[[9, 3], [7, 0]], [[[5, 1], [3, 8]], 9]]
                 [[[3, 9], [[5, 9], 2]], [[7, 2], 1]]
                 [[1, [[3, 0], [7, 6]]], [7, [8, 1]]]
                 [0, [6, [[7, 1], [1, 1]]]]
                 [[4, [[5, 0], [2, 1]]], [[[8, 8], [8, 1]], 7]]
                 [[[[9, 3], [4, 3]], 4], [7, 5]]
                 [[9, [[7, 4], [8, 3]]], [[[1, 9], 7], [[1, 6], [3, 1]]]]
                 [[6, 9], [5, [0, [5, 1]]]]
                 [[[8, 7], 3], [[4, 8], [0, 7]]]
                 [[[[3, 1], 2], [[1, 6], [4, 3]]], [0, 6]]
                 [[5, [[5, 4], 3]], [[8, 8], 9]]
                 [[5, [3, [4, 5]]], [[2, [6, 0]], [6, 1]]]
                 [[[[9, 5], 3], 6], [[8, [1, 9]], [[5, 2], 5]]]
                 [[[7, 5], [[3, 6], 4]], [6, [[5, 1], [0, 1]]]]
                 [[1, [[4, 8], [1, 3]]], 7]
                 [[4, [[4, 0], 5]], [[[6, 2], 7], [[4, 8], [4, 9]]]]
                 [[[[2, 3], [0, 9]], [7, 2]], [4, 5]]
                 [[[[7, 7], [8, 0]], [7, 7]], [[[6, 6], [3, 2]], [4, [4, 3]]]]
                 [[[[8, 7], 6], [[5, 5], 0]], [[6, [7, 3]], [[4, 1], [1, 7]]]]
                 [[[2, [2, 2]], [[5, 2], 1]], [[9, [9, 2]], 6]]
                 [[[[1, 7], 6], [[8, 8], 5]], [6, [1, [1, 7]]]]
                 [[[[8, 6], [3, 2]], [[5, 2], [2, 0]]], [[[8, 7], 2], [[5, 5], 2]]]
                 [[[8, [9, 0]], [[9, 5], [7, 5]]], [[5, 1], [[1, 1], [4, 6]]]]
                 [5, [9, [[0, 2], 7]]]
                 [8, [[0, [4, 9]], [[7, 4], 9]]]
                 [[[[2, 9], 5], [[0, 6], [6, 6]]], [[0, 6], [[4, 2], [9, 9]]]]
                 [7, [[[4, 3], 3], [[5, 4], [6, 0]]]]
                 [[0, [8, [1, 1]]], 5]
                 [[[1, 8], [[4, 6], [9, 7]]], [[[6, 6], [2, 6]], [4, 3]]]
                 [[0, [[7, 5], [9, 9]]], [[9, 7], [6, 2]]]
                 [[[9, [3, 0]], [[1, 4], 0]], [[1, 1], 1]]
                 [[[0, 7], [[3, 0], 8]], [[6, [8, 0]], [[4, 5], [4, 0]]]]
                 [[[[2, 9], [4, 2]], [5, [9, 3]]], [4, [2, [3, 4]]]]
                 [[[1, [7, 3]], [[5, 7], 0]], [6, [[6, 5], 2]]]
                 [4, 5]
                 [[7, 9], [6, [[6, 5], [1, 0]]]]
                 [[4, [[7, 5], 8]], [[4, 0], [[6, 6], [0, 4]]]]
                 [[[9, [7, 7]], [[4, 2], 7]], 4]
                 [[0, [0, 3]], 5]])))
  )
