(ns day10
  (:require [clojure.string :as str]))

(def small-input "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732")
(def input (slurp (format "src/%s.txt" *ns*)))

(defn s->pos->h [s]
  (->> (str/split-lines s)
       (mapcat (fn [y l] (keep-indexed (fn [x c] [[x y] (- (int c) (int \0))]) l))
               (range))
       (into {})))

(comment

  (let [pos->h (s->pos->h input)]
    (->> pos->h
         (filterv (comp zero? second))
         (mapv (fn [[start-pos]]
                 (loop [q [start-pos] seen #{} trail-paths 0]
                   (if (seq q)
                     (let [pos (peek q)
                           h (get pos->h pos)]
                       #_(clojure.pprint/pprint [pos h trail-paths])
                       (recur (cond-> (pop q)
                                (not= 9 h) (into (comp (map #(mapv + pos %))
                                                       (remove seen)
                                                       (filter #(= (inc h) (pos->h % 0))))
                                                 [[0 1] [1 0] [-1 0] [0 -1]]))
                              (conj seen pos)
                              (cond-> trail-paths
                                (= 9 h) inc)))
                     trail-paths))))
         (reduce +)))
  ; 688

  (let [pos->h (s->pos->h input)]
    (->> pos->h
         (filterv (comp zero? second))
         (mapv (fn [[start-pos]]
                 (loop [q [[start-pos #{}]] trail-paths 0]
                   (if (seq q)
                     (let [[pos seen] (peek q)
                           seen (conj seen pos)
                           h (get pos->h pos)]
                       #_(clojure.pprint/pprint [pos h trail-paths])
                       (recur (cond-> (pop q)
                                (not= 9 h) (into (comp (map #(mapv + pos %))
                                                       (remove seen)
                                                       (filter #(= (inc h) (pos->h % 0)))
                                                       (map #(vector % seen)))
                                                 [[0 1] [1 0] [-1 0] [0 -1]]))
                              (cond-> trail-paths
                                (= 9 h) inc)))
                     trail-paths))))
         (reduce +)))
  ; 1459
  )
