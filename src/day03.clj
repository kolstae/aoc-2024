(ns day03
  (:require [clojure.string :as str]))

(def small-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def small-input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
(def input (slurp (format "src/%s.txt" *ns*)))


(comment

  (->> (re-seq #"mul\((\d+),(\d+)\)" input #_ small-input)
       (map (comp (partial mapv parse-long) next))
       (mapv (partial apply *))
       (reduce +))
  ; 159833790

  (->> (loop [ss (re-seq #"mul\(\d+,\d+\)|do\(\)|don't\(\)" input #_small-input-2) ms [] mode :do]
         (if (seq ss)
           (let [[new-ms more] (split-with #(str/starts-with? % "mul") ss)
                 ms (if (= mode :do) (into ms new-ms) ms)
                 new-mode (if (= (first more) "do()") :do :don't)]
             (recur (next more) ms new-mode) )
           ms))
       (mapv (comp (partial mapv parse-long) (partial re-seq #"\d+")))
       (mapv (partial apply *))
       (reduce +))
  ; 89349241
  )
