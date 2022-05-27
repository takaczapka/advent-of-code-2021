(ns advent-of-code-2021.22
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [clojure.zip :as zip]))

(defn read-input [file]
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

;;;; part 2
(defn bbb [[x1 x2] [x3 x4]]
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
    (sorted? [x3 x1 x2 x4]) (do (prn :what?)
                                [0 0])))



(defn do-magic [[x1 y1 z1] [x2 y2 z2]]
  (let [res (cond
              (and (is-contained y1 y2) (is-contained z1 z2)) [(aasd x1 x2) y1 z1]
              (and (is-contained x1 x2) (is-contained z1 z2)) [x1 (aasd y1 y2) z1]
              (and (is-contained x1 x2) (is-contained y1 y2)) [x1 y1 (aasd z1 z2)]
              :else [x1 y1 z1])]
    (prn :acc [x1 y1 z1])
    (prn :step [x2 y2 z2])
    (prn :res res)
    res))

(defn find-boundaries [ss]
  (reduce (fn [[x' y' z'] {:keys [sig x y z]}]
            (if sig
              [(bbb x' x) (bbb y' y) (bbb z' z)]
              (do-magic [x' y' z'] [x y z])
              ;[x' y' z']
              ))
          []
          ss))

(defn do-naive-little-better [steps]
  (let [[[x1 x2] [y1 y2] [z1 z2]] (find-boundaries steps)
        _   (prn :bdrs [[x1 x2] [y1 y2] [z1 z2]])
        res (for [x (range (dec x1) (inc x2))
                  y (range (dec y1) (inc y2))
                  z (range (dec z1) (inc z2))]
              (is-on [x y z] steps))]
    (count (filter true? res))))


(defn isect? [[a-x1 a-x2] [b-x1 b-x2]]
  (or
    (and (>= a-x1 b-x1) (<= a-x1 b-x2))
    (and (>= a-x2 b-x1) (<= a-x2 b-x2))
    (and (<= a-x1 b-x1) (>= a-x2 b-x2))))

(defn intersects? [c1 c2]

  (every? true? (map isect? c1 c2))
  )

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
  (filter-valid (concat (diff-x c1 c2)
                        (diff-y c1 c2)
                        (diff-z c1 c2)
                        )))

(defn diff-all [cs c]
  (mapcat #(diff c %) cs))

(defn diff-onllyy [cs new-addition sig]
  (set (reduce (fn [acc n]
                 (set (diff-all acc n)))
               [new-addition]
               cs)))

(defn calc-area [[[x1 x2] [y1 y2] [z1 z2]]]
  (* (inc (- x2 x1)) (inc (- y2 y1)) (inc (- z2 z1)))
  )

(defn calc-areas [cs]
  ;(prn :calc-cs cs)
  ;(prn :calc-res (apply + (map calc-area cs)))
  (apply + (map calc-area cs)))

(defn do-it-good [is]
  (let [ris     (reverse is)
        sig (:sig (first ris))
        first-c (if sig
                  (calc-areas [[(:x (first ris)) (:y (first ris)) (:z (first ris))]])
                  0)]

    (reduce (fn [{:keys [aaa bbb]} {:keys [sig x y z]}]
              (let [
                    ;_      (prn :sig sig)
                    ;_      (prn :accnn [x y z])
                    ;_      (prn :acc acc)
                    d-only (diff-onllyy bbb [x y z] sig)
                    ;_      (prn :d-only d-only)
                    a      (if sig
                             (calc-areas d-only))
                    ]
                {:aaa (if a (+ a aaa) aaa) :bbb (concat bbb d-only)})
              )
            {:aaa first-c
             :bbb [[(:x (first ris)) (:y (first ris)) (:z (first ris))]]}
            (rest ris)

            ))
  )

(deftest aoc-21-2

  (diff [[10 10] [10 10] [10 10]] [[9 11] [9 11] [9 11]])
  ;(diff [[9 11] [9 11] [9 11]] [[10 10] [10 10] [10 10]] )

  ;(prn :xxxx (diff-after-shave [[] [16 20] [0 10] [0 10]]))
  ;;
  (is (= [[[0 4] [0 10] [0 10]] [[16 20] [0 10] [0 10]]]
         (diff [[5 15] [0 10] [0 10]] [[0 20] [0 10] [0 10]])))
  (is (= (list)
         (diff [[0 20] [0 10] [0 10]] [[5 15] [0 10] [0 10]])))

  (is (= [[[0 10] [0 4] [0 10]] [[0 10] [16 20] [0 10]]]
         (diff [[0 10] [5 15] [0 10]] [[0 10] [0 20] [0 10]])))

  (is (= [[[0 10] [0 10] [0 4]] [[0 10] [0 10] [16 20]]]
         (diff [[0 10] [0 10] [5 15]] [[0 10] [0 10] [0 20]])))
  ;
  ;
  (is (= [[[0 20] [-2 -1] [0 10]]
          [[0 20] [11 12] [0 10]]
          [[0 4] [-2 12] [0 10]]
          [[16 20] [-2 12] [0 10]]]
         (diff [[5 15] [0 10] [0 10]] [[0 20] [-2 12] [0 10]])))

  ;(prn :ccc (diff [[5 15] [0 10] [0 10]] [[0 20] [0 12] [0 10]]))
  ;(prn :ccc (diff [[5 15] [0 10] [0 10]] [[0 20] [0 12] [-5 15]]))
  ;(prn :ccc (diff-after-shave (diff [[5 15] [0 10] [0 10]] [[0 20] [0 12] [-5 15]])))

  ;(prn :ccc  (diff [[5 15] [0 10] [0 10]] [[0 20] [-2 12] [-5 15]]))

  ;
  ;
  ;
  ;(is (intersects? [[0 10] [0 10] [0 10]] [[0 10] [9 25] [9 25]]))
  ;(is (intersects? [[0 10] [0 10] [0 10]] [[0 10] [10 25] [9 25]]))
  ;(is (intersects? [[0 10] [0 10] [0 10]] [[10 11] [10 25] [10 25]]))
  ;(is (false? (intersects? [[0 10] [0 10] [0 10]] [[0 10] [20 25] [20 25]])))
  ;(is (false? (intersects? [[0 10] [20 25] [20 25]] [[0 10] [0 10] [0 10]])))
  ;(is (false? (intersects? [[0 10] [0 10] [0 10]] [[10 10] [20 25] [20 25]])))
  ;(is (false? (intersects? [[0 10] [0 10] [0 10]] [[10 10] [20 25] [20 25]])))
  ;(is (false? (intersects? [[0 10] [0 10] [0 10]] [[20 21] [20 21] [20 21]])))
  ;

  ;(is (= 39 (do-naive-little-better (read-input "22-1.txt"))))
  ;
  ;(is (= 590784 (do-naive-little-better (read-input "22-2.txt"))))
  ;;;
  ;(is (= 606484 (do-naive-little-better (read-input "22-3.txt"))))

  (prn :iii
       (:aaa (do-it-good (read-input "22-1.txt"))))
  (prn :iii
       (:aaa (do-it-good (read-input "22-2.txt"))))
  (prn :iii
       (:aaa (do-it-good (read-input "22-3.txt"))))

  (prn :iii
       (:aaa (do-it-good (read-input "22-5.txt"))))

  (prn :iii
       (:aaa (do-it-good (read-input "22-4.txt"))))
  ; not 3368686838866479
  ; not 1166930839851764

  ;
  ;(prn :aas (diff-after-shave (diff [[10 10] [10 10] [10 10]] [[9 11] [9 11] [9 11]])))
  ;;(prn :aas (diff-after-shave (diff-after-shave (diff [[9 11] [9 11] [9 11]] [[11 13] [11 13] [11 13]] ))))
  ;(prn :aas (diff [[11 13] [11 13] [11 13]] [[10 12] [10 12] [10 12]]))
  ;;(prn :aas (diff-after-shave (diff [[11 13] [11 13] [11 13]] [[10 12] [10 12] [10 12]])))
  ;
  ;(prn :aaaa (diff [[13 13] [11 13] [11 13]] [[11 13] [13 13] [11 13]]))
  ;(prn :aaaa (diff [[11 13] [13 13] [11 13]] [[13 13] [11 13] [11 13]]))

  ;(prn :xxx (calc-area [[10 12] [10 12] [10 12]]))
  ;(intersects? [[9 10] [9 11] [9 11]] [[9 10] [9 11] [9 10]])

  )

;(is (= [[[0 15] [-5 15] [-5 12]]]
;      (make-cubes [[[0 10] [0 10] [0 10]]] {:sig true [0 15] [-5 15] [-5 12]})))

;(let [steps (read-input "22-4.txt")]
;
;
;  )

