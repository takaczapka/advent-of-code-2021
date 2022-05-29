(ns advent-of-code-2021.10
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [advent-of-code-2021.common :as common]))

(def opening #{"<" "[" "{" "("})
(def closing #{">" "]" "}" ")"})

(def m {"<" ">"
        "{" "}"
        "[" "]"
        "(" ")"})

(defn good-or-break [line]
  (let [a (reduce (fn [acc n]
                    (cond
                      (opening n) (conj acc n)
                      (and (closing n) (= n (get m (first acc)))) (rest acc)
                      :else (reduced {:found n})))
                  '() (str/split line #""))]
    (when (map? a)
      (:found a))))

(defn good-or-fill [line]
  (let [a (reduce (fn [acc n]
                    (cond
                      (opening n) (conj acc n)
                      (and (closing n) (= n (get m (first acc)))) (rest acc)
                      :else (reduced :not-found)))
                  '() (str/split line #""))]
    (when (seq? a)
      (map m a))))

(defn aoc-10-1 [i]
  (let [calc-points (fn [ls]
                      (apply + (map (fn [[k v]] (* v k)) (frequencies ls))))]
    (->> i
         (map good-or-break)
         (remove nil?)
         (map {"}" 1197
               ")" 3
               "]" 57
               ">" 25137})
         calc-points)))

(defn aoc-10-2 [i]
  (let [points      {"}" 3
                     ")" 1
                     "]" 2
                     ">" 4}
        calc-points (fn [ls]
                      (reduce (fn [acc l]
                                (+ (* acc 5) l))
                              0 ls))
        res         (->> i
                         (map good-or-fill)
                         (remove nil?)
                         (map #(map points %))
                         (map calc-points)
                         sort)]
    (nth res (quot (count res) 2))))

(deftest aoc-10
  (is (= 26397 (aoc-10-1 ["[({(<(())[]>[[{[]{<()<>>"
                          "[(()[<>])]({[<{<<[]>>("
                          "{([(<{}[<>[]}>{[]{[(<()>"
                          "(((({<>}<{<{<>}{[]{[]{}"
                          "[[<[([]))<([[{}[[()]]]"
                          "[{[{({}]{}}([{[{{{}}([]"
                          "{<[[]]>}<{[{[{[]{()[[[]"
                          "[<(<(<(<{}))><([]([]()"
                          "<{([([[(<>()){}]>(<<{{"
                          "<{([{{}}[<[[[<>{}]]]>[]]"])))

  (let [i-2 (common/read-input "10.txt")]
    (is (= 240123 (aoc-10-1 i-2)))
    (is (= 3260812321 (aoc-10-2 i-2)))))
