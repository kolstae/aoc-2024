(ns day16
  (:require [clojure.string :as str]))

(def small-input "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############")
(def input (slurp (format "src/%s.txt" *ns*)))

(defn s->pos-c [ss]
  (->> ss
       (mapcat (fn [y l] (keep-indexed (fn [x c] (when (not= c \#) [[y x] c])) l))
               (range))
       (into {})))

(defn dir->score-dirs [pts dir]
  (case dir
    :E [[pts :E] [(+ pts 1000) :N] [(+ pts 1000) :S] [(+ pts 2000) :W]]
    :W [[pts :W] [(+ pts 1000) :N] [(+ pts 1000) :S] [(+ pts 2000) :E]]
    :N [[pts :N] [(+ pts 1000) :W] [(+ pts 1000) :E] [(+ pts 2000) :S]]
    :S [[pts :S] [(+ pts 1000) :W] [(+ pts 1000) :E] [(+ pts 2000) :N]]))

(defn move [dir pos]
  (case dir
    :N (update pos 0 dec)
    :S (update pos 0 inc)
    :E (update pos 1 dec)
    :W (update pos 1 inc)))

(comment

  (let [ps (s->pos-c (str/split-lines input))
        start-pos (ffirst (filter (comp #{\S} val) ps))
        end-pos (ffirst (filter (comp #{\E} val) ps))]
    (loop [q [[0 :E start-pos]] visited #{}]
      #_(clojure.pprint/pprint [q visited])
      (if (seq q)
        (let [[pts dir pos :as v] (apply min-key first q)]
          (if (= end-pos pos)
            pts
            (recur (->> (dir->score-dirs (inc pts) dir)
                        (into (vec (remove #{v} q))
                              (keep (fn [[_ dir :as v]]
                                      (let [p (move dir pos)]
                                        (when (and (ps p) (not (visited p)))
                                          (conj v p)))))))
                   (conj visited pos))))
        :|)))
  ; 135536

  (let [ps (s->pos-c (str/split-lines input))
        start-pos (ffirst (filter (comp #{\S} val) ps))
        end-pos (ffirst (filter (comp #{\E} val) ps))]
    (loop [q [[0 :E start-pos #{start-pos}]] path #{} score Long/MAX_VALUE]
      (if (seq q)
        (let [[pts dir pos visited :as v] (apply min-key first q)]
          (if (< score pts)
            (count path)
            (if (= end-pos pos)
              (recur (vec (remove #{v} q))
                     (into path visited)
                     pts)
              (recur (->> (dir->score-dirs (inc pts) dir)
                          (into (vec (remove #{v} q))
                                (keep (fn [[pts dir :as v]]
                                        (when (< pts score)
                                          (let [p (move dir pos)]
                                            (when (and (ps p) (not (visited p)))
                                              (conj v p (conj visited p)))))))))
                     path
                     score))))
        :|)))
  ;
  )
