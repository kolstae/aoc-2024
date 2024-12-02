(ns day01
  (:require [clojure.string :as str]))

(def small-input "3   4\n4   3\n2   5\n1   3\n3   9\n3   3")
(def input (slurp (format "src/%s.txt" *ns*)))

(comment

  (let [#_#_input small-input
        ns (->> (str/split-lines input)
                (mapcat #(str/split % #"\s+"))
                (mapv parse-long))]
    (->> (map (comp abs (partial -))
              (sort (take-nth 2 ns))
              (sort (take-nth 2 (next ns))))
         (reduce +)))
  ; 1506483

  (let [#_#_input small-input
        ns (->> (str/split-lines input)
                (mapcat #(str/split % #"\s+"))
                (mapv parse-long))
        fs (frequencies (take-nth 2 (next ns)))]

    (->> (map (fn [i] (* i (fs i 0)))
              (take-nth 2 ns))
         (reduce +)))
  ; 23126924
  )
