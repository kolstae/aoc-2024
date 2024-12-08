(ns day08
  (:require [clojure.string :as str]))

(def small-input "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............")
(def input (slurp (format "src/%s.txt" *ns*)))

(comment

  (let [lines (str/split-lines input)
        size (count lines)
        inside? (fn [p] (and (not-any? neg? p) (not-any? (partial <= size) p)))]
    (->> lines
         (mapcat (fn [y l] (keep-indexed (fn [x c] (when-not (#{\. \#} c) [[x y] c])) l))
                 (range))
         (group-by second)
         vals
         (mapcat (fn [kvs]
                   (let [ps (mapv first kvs)]
                     (for [a (butlast ps) b (next ps) :when (not= a b)]
                       [(mapv + a (mapv - a b)) (mapv + b (mapv - b a))]))))
         (mapcat identity)
         (filter inside?)
         set
         count))
  ; 301


  (let [lines (str/split-lines input)
        size (count lines)
        inside? (fn [p] (and (not-any? neg? p) (not-any? (partial <= size) p)))]
    (->> lines
         (mapcat (fn [y l] (keep-indexed (fn [x c] (when-not (#{\. \#} c) [[x y] c])) l))
                 (range))
         (group-by second)
         vals
         (mapcat (fn [kvs]
                   (let [ps (mapv first kvs)]
                     (for [a (butlast ps) b (next ps) :when (not= a b)]
                       (concat (take-while inside? (iterate (partial mapv + (mapv - a b)) a))
                               (take-while inside? (iterate (partial mapv + (mapv - b a)) b)))))))
         (mapcat identity)
         set
         count))
  ; 1019
  )
