(ns advent-of-code-2021.15
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [file]
  (mapv (fn [v] (mapv #(- (int %) 48) (vec v)))
        (->
          (io/resource file)
          (slurp)
          (str/split #"\n"))))

{:path  []
 :score []}

(defn next-move [b {:keys [path score]} xy]
  (if-let [v (get-in b (reverse xy))]
    {:path (cons xy path) :score (+ score v)})
  )

(defn worth-going [b scoring score-so-far xy]
  (if-let [v (get-in b (reverse xy))]
    (< (+ score-so-far v) (get-in scoring (reverse xy))))
  )

(defn update-scoring [mvs scoring]
  (reduce (fn [acc {:keys [path score]}]
            (if (get-in scoring (reverse (first path)))
              (assoc-in acc (reverse (first path)) score)
              acc))
          scoring
          mvs))

(defn next-moves [b {:keys [path score] :as pp} scoring]
  (let [[x y] (first path)
        mm                (remove #(.contains path %) [[(inc x) y] [x (inc y)] [(dec x) y]])
        nm-better-scoring (filter #(worth-going b scoring score %) mm)
        moves             (remove nil? (map #(next-move b pp %) nm-better-scoring))]
    [moves (update-scoring moves scoring)]
    ))

(defn find-the-smallest-next-step [sps])

(defn next-step [b [ps scoring]]
  (let [sorted   ps
        smallest (first sorted)
        others   (rest sorted)
        [nm updated-scoring] (next-moves b smallest scoring)
        aaaa     (sort-by :score (concat nm others))
        ;; choose only the best scores from paths
        opt      (first (:path (first aaaa)))
        score    (:score (first aaaa))
        ]
    [(remove #(and (= (first (:path %)) opt) (not= score (:score %)))
             aaaa) updated-scoring]))

(defn dddd [b]
  (let [start   {:path [[0 0]] :score 0}
        scoring (vec (repeat (count b) (vec (repeat (count (first b)) 9999999))))
        nm      (drop-while
                  #(not= (first (:path (first (first %))))
                         [(dec (count (first b))) (dec (count b))])
                  (iterate #(next-step b %) [[start] scoring]))
        res     (first (first (first nm)))
        _       (prn (:score res))
        ])
  )

(defn do-f [b]
  (iterate
    (fn [zz] (mapv (fn [z] (mapv #(if (= % 9) 1 (inc %)) z)) zz))
    b)
  )


(defn join-rows [a b c d e]
  (mapv (fn [x y z c d] (vec (concat x y z c d))) a b c d e)
  )


(deftest aoc-15
  (let [input    (read-input "15.txt")
        [a b c d e f g h i j] (take 10 (do-f input))
        now-this (vec (concat
                        (join-rows a b c d e)
                        (join-rows b c d e f)
                        (join-rows c d e f g)
                        (join-rows d e f g h)
                        (join-rows e f g h i)
                        ))
        _ (prn now-this)
        _        (prn (dddd now-this))
        _        (prn (dddd input))
        ])

  )







;(defn calc-score [bb]
;  (let [b (assoc-in bb [0 0] 0)]
;
;    (let [vs (rest (for [x (range 0 (count (first b)))
;                   y (range 0 (count b))]
;               [x y]))]
;      (reduce
;        (fn [acc [x y]]
;          (let [sc (get-in b [y x])
;                _ (prn :sc sc)
;                sc-right (get-in acc [y (inc x)] 100)
;                sc-left (get-in acc [y (dec x)] 100)
;                sc-up (get-in acc [(dec y) x] 100)]
;            (assoc-in acc [y x] (+ sc (min sc-left sc-up)))
;            )
;          )
;        b
;        vs)
;      )
;
;
;
;    ))

