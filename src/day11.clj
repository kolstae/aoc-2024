(ns day11
  (:require [clojure.string :as str]))

(def small-input "125 17")
(def input (slurp (format "src/%s.txt" *ns*)))

(defn next-ns [n]
  (if (zero? n)
    [1]
    (let [s (str n)]
      (if (even? (count s))
        [(parse-long (subs s 0 (/ (count s) 2))) (parse-long (subs s (/ (count s) 2)))]
        [(* n 2024)]))))

(declare memoize-stones-for-i-n)

(defn stones-for-i-n [i n]
  (if (zero? i)
    1
    (reduce + (map (partial memoize-stones-for-i-n (dec i)) (next-ns n)))))

(def memoize-stones-for-i-n (memoize stones-for-i-n))

(comment

  (->> (str/split input #"\s")
       (mapv parse-long)
       (iterate #(into [] (mapcat next-ns) %))
       (drop 25)
       (map count)
       first)
  ; 235850

  (->> (str/split input #"\s")
       (mapv parse-long)
       (mapv (partial memoize-stones-for-i-n 75))
       (reduce +))
  ; 279903140844645
  )
