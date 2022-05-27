(ns advent-of-code-2021.10
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def opening #{\< \[ \{ \(})
(def closing #{\> \] \} \)})


(defn read-input [file]
  (->
    (io/resource file)
    (slurp)
    (str/split #"\n")))



(def opening #{"<" "[" "{" "("})
(def closing #{">" "]" "}" ")"})

(def m {"<" ">"
        "{" "}"
        "[" "]"
        "(" ")"})

(defn good-or-break [line]
  (let [a (reduce (fn [acc n]
                    (cond
                      (some #{n} opening) (conj acc n)
                      (and (some #{n} closing) (= (get m (first acc)) n)) (rest acc)
                      :else (reduced {:boom n})))
                  '() (str/split line #""))]
    (when (map? a)
      (:boom a))))


(defn good-or-fill [line]
  (let [a (reduce (fn [acc n]
                    (cond
                      (some #{n} opening) (conj acc n)
                      (and (some #{n} closing) (= (get m (first acc)) n)) (rest acc)
                      :else (reduced {:boom n})
                      ))
                  '() (str/split line #""))]
    (when (seq? a)
      (map m a))))

(def points {
             "}" 1197
             ")" 3
             "]" 57
              ">" 25137
             })

(def points-2 {
             "}" 3
             ")" 1
             "]" 2
             ">" 4
             })

(defn calc-points [ls]
  (reduce (fn [acc l]
            (+ (* acc 5) l)
            )
          0 ls))

(deftest aoc-10


  (let [
        i [
           "[({(<(())[]>[[{[]{<()<>>"
           "[(()[<>])]({[<{<<[]>>("
           "{([(<{}[<>[]}>{[]{[(<()>"
           "(((({<>}<{<{<>}{[]{[]{}"
           "[[<[([]))<([[{}[[()]]]"
           "[{[{({}]{}}([{[{{{}}([]"
           "{<[[]]>}<{[{[{[]{()[[[]"
           "[<(<(<(<{}))><([]([]()"
           "<{([([[(<>()){}]>(<<{{"
           "<{([{{}}[<[[[<>{}]]]>[]]"
           ]
        i (read-input  "10.txt")
        ;_ (prn i)
        ]
    (let [ress
          (->>
            (map good-or-fill i)
            (remove nil?)
            (map (fn [l] (map points-2 l)))
            (map calc-points)
            sort
            )
          ]
      (prn ress)
      (prn (nth ress (quot (count ress) 2)))
      ))

  )
