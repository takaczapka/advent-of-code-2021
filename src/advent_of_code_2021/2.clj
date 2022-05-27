(ns advent-of-code-2021.2
  (:require [advent-of-code-2021.common :as common]
            [clojure.test :refer :all]
            [clojure.string :as str]))



(defn ac-2 [input]
  (let [{:keys [forward depth]}
        (reduce (fn [{:keys [aim] :as acc} n]
                  (let [[direction p] (str/split n #" ")
                        step (parse-long p)]
                    (case direction
                      "forward" (-> acc
                                    (update :forward + step)
                                    (update :depth + (* aim step)))
                      "up" (update acc :aim - step)
                      "down" (update acc :aim + step))))
                {:forward 0 :depth 0 :aim 0}
                input)]
    (* forward depth)))

(deftest ac-2-test
  (is (= 1840311528
         (->> (common/read-input "2.txt")
              (ac-2))))

  (is (= 900
         (ac-2 ["forward 5"
                "down 5"
                "forward 8"
                "up 3"
                "down 8"
                "forward 2"]))))
