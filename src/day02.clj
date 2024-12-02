(ns day02
  (:require [clojure.string :as str]))

(def small-input "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9")
(def input (slurp (format "src/%s.txt" *ns*)))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv (comp (partial mapv parse-long) #(str/split % #"\s+")))))

(defn safe? [ns]
  (let [ds (mapv - ns (next ns))]
    (or (every? (every-pred #{1 2 3} pos?) ds)
        (every? (every-pred #{-1 -2 -3} neg?) ds))))

(comment

  (->> (parse-input input #_small-input)
       (filter safe?)
       count)
  ; 572

  (->> (parse-input input #_small-input)
       (filter (fn [ns]
                 (or (safe? ns)
                     (let [len (count ns)]
                       (->> (range len)
                            (map (fn [i] (into (subvec ns 0 i) (subvec ns (inc i) len))))
                            (drop-while (complement safe?))
                            first)))))
       count)
  ; 612
  )
