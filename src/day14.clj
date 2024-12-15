(ns day14
  (:require [clojure.string :as str]))

(def small-input "p=0,4 v=3,-3\np=6,3 v=-1,-3\np=10,3 v=-1,2\np=2,0 v=2,-1\np=0,0 v=1,3\np=3,0 v=-2,-2\np=7,6 v=-1,-3\np=3,0 v=-1,-2\np=9,3 v=2,3\np=7,3 v=-1,2\np=2,4 v=2,-3\np=9,5 v=-3,-3")
(def input (slurp (format "src/%s.txt" *ns*)))

(defn move-robot [dim [p v]]
  [(mapv mod (mapv + p v) dim) v])

(comment

  (let [[input width height] [input 101 103]
        #_#_[input width height] [small-input 11 7]
        w_half (long (/ width 2))
        h_half (long (/ height 2))]
    (->> (str/split-lines input)
         (mapv (comp vec (partial partitionv 2) (partial mapv parse-long) (partial re-seq #"-?\d+")))
         (iterate (fn [rs] (mapv (partial move-robot [width height]) rs)))
         (drop 100)
         first
         (mapv first)
         (group-by (fn [[x y]]
                     (vector
                       (cond
                         (< y h_half) :upper
                         (>= y (- height h_half)) :lower)
                       (cond
                         (< x w_half) :left
                         (>= x (- width w_half)) :right))))
         (remove (comp (partial some nil?) first))
         (map (comp count second))
         (reduce *)))
  ; 220971520

  (let [[input width height] [input 101 103]
        #_#_[input width height] [small-input 11 7]
        ds (vec (for [x (range -1 2) y (range -1 2) :when (not= [0 0] [x y])] [x y]))]
    (->> (str/split-lines input)
         (mapv (comp vec (partial partitionv 2) (partial mapv parse-long) (partial re-seq #"-?\d+")))
         (iterate (fn [rs] (mapv (partial move-robot [width height]) rs)))
         (keep-indexed (fn [i ps] [i (into [] (map first) ps)]))
         (filter (fn [[_ ps]] (apply distinct? ps)))
         (map (fn [[i ps]]
                (let [ps (set ps)]
                  [i
                   (vec (for [y (range height)]
                          (str/join (for [x (range width)]
                                      (if (ps [x y]) \X \.)))))])))
         first))
  ; 6355
  )
