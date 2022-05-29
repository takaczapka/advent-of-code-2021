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

(defn next-move [b {:keys [path score]} xy]
  (if-let [v (get-in b (reverse xy))]
    {:path (cons xy path) :score (+ score v)}))

(defn worth-going [b scoring score-so-far xy]
  (if-let [v (get-in b (reverse xy))]
    (< (+ score-so-far v) (get-in scoring (reverse xy)))))

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
    [moves (update-scoring moves scoring)]))

(defn next-step [b [ps scoring]]
  (let [sorted   ps
        smallest (first sorted)
        others   (rest sorted)
        [nm updated-scoring] (next-moves b smallest scoring)
        scores   (sort-by :score (concat nm others))
        ;; choose only the best scores from paths
        opt      (first (:path (first scores)))
        score    (:score (first scores))]
    [(remove #(and (= (first (:path %)) opt) (not= score (:score %)))
             scores) updated-scoring]))

(defn dddd [b]
  (let [start   {:path [[0 0]] :score 0}
        scoring (vec (repeat (count b) (vec (repeat (count (first b)) 9999999))))
        nm      (drop-while
                  #(not= (first (:path (first (first %))))
                         [(dec (count (first b))) (dec (count b))])
                  (iterate #(next-step b %) [[start] scoring]))
        res     (first (first (first nm)))]
    (:score res)))

(defn do-f [b]
  (iterate
    (fn [zz] (mapv (fn [z] (mapv #(if (= % 9) 1 (inc %)) z)) zz))
    b)
  )

(defn join-rows [a b c d e]
  (mapv
    (fn [x y z c d] (vec (concat x y z c d)))
    a b c d e))

(deftest aoc-15
  (let [input    (read-input "15.txt")
        [a b c d e f g h i] (take 9 (do-f input))
        now-this (vec (concat
                        (join-rows a b c d e)
                        (join-rows b c d e f)
                        (join-rows c d e f g)
                        (join-rows d e f g h)
                        (join-rows e f g h i)))]
    ; aoc-15-1
    (is (= 441 (dddd input)))
    ; aoc-15-2
    (is (= 2849 (dddd now-this)))))
