(ns advent-of-code-2021.2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn ac-2 [input]

  (let [{:keys [forward depth]}
        (reduce (fn [{:keys [forward depth aim] :as acc} n]
                  (let [[dir p] (str/split n #" ")
                        step (Integer/parseInt p)]
                    (case dir
                      "forward" (assoc acc :forward (+ forward step) :depth (+ depth (* aim step)))
                      "up" (assoc acc :aim (- aim step))
                      "down" (assoc acc :aim (+ aim step))
                      )))
                {:forward 0 :depth 0 :aim 0}
                input)]
    (* forward depth)))

(defn read-input [file]
  (->
    (io/resource file)
    (slurp)
    (str/split #"\n")))

(comment
  (->> (read-input "2.txt")
       (ac-2))


  (ac-2 ["forward 5"
         "down 5"
         "forward 8"
         "up 3"
         "down 8"
         "forward 2"])

  )

