(ns day07
  (:require [clojure.string :as str]))

(def small-input "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20")
(def input (slurp (format "src/%s.txt" *ns*)))

(defn split-ns [s]
  (->> (re-seq #"\d+" s) (mapv parse-long)))

(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [more (cart (rest colls))
          x (first colls)]
      (cons x more))))

(defn finder-for-ops [ops]
  (fn [[x a & ns]]
    (first
      (for [ops (cart (repeat (count ns) ops))
            :let [n (->> (mapv vector ops ns)
                         (reduce (fn [a [op b]] (op a b)) a))]
            :when (= n x)]
        x))))

(defn || [^long a ^long b]
  (+ b (loop [mag 10]
         (if (> mag b)
           (* a mag)
           (recur (* 10 mag))))))


(comment

  (->> (str/split-lines input)
       (mapv split-ns)
       (keep (finder-for-ops [* +]))
       (reduce +))
  ; 1038838357795

  (->> (str/split-lines input)
       (mapv split-ns)
       (keep (finder-for-ops [* + ||]))
       (reduce +)
       time)
  ; 254136560217241
  )
