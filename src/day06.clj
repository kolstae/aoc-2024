(ns day06
  (:require [clojure.string :as str]))

(def small-input "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#...")
(def input (slurp (format "src/%s.txt" *ns*)))


(defn move [dir p]
  (case dir
    :up (update p 1 dec)
    :down (update p 1 inc)
    :left (update p 0 dec)
    :right (update p 0 inc)))

(defn next-dir [dir]
  (case dir
    :up :right
    :right :down
    :down :left
    :left :up))

(defn walk [size obstacle pos dir]
  (->> [pos dir]
       (iterate (fn [[pos dir]]
                  (when-not (or (some zero? pos) (some (partial = (dec size)) pos))
                    (let [next-pos (move dir pos)]
                      (if (obstacle next-pos)
                        [pos (next-dir dir)]
                        [next-pos dir])))))
       (take-while some?)))

(comment

  (let [lines (str/split-lines input)
        size (count lines)
        pos (mapcat (fn [y l] (keep-indexed (fn [x c] (when (#{\# \^} c) [[x y] c])) l))
                    (range) lines)
        start-pos (ffirst (filter (comp #{\^} second) pos))
        obstacle (into #{} (comp (remove (comp #{\^} second)) (map first)) pos)]
    (->> (walk size obstacle start-pos :up)
         (into #{} (map first))
         count))
  ; 5564

  (let [lines (str/split-lines input)
        size (count lines)
        pos (mapcat (fn [y l] (keep-indexed (fn [x c] (when (#{\# \^} c) [[x y] c])) l))
                    (range) lines)
        start-pos (ffirst (filter (comp #{\^} second) pos))
        obstacle (into #{} (comp (remove (comp #{\^} second)) (map first)) pos)
        loop? (fn [been new-obstacle start-pos dir]
                (when (and (not (obstacle new-obstacle))
                           ; Don't place obstacle in our trodden path
                           (not-any? #(been [new-obstacle %]) [:up :down :left :right]))
                  (loop [[pd & more] (walk size (conj obstacle new-obstacle) start-pos dir) been been]
                    (when pd
                      (if (been pd)
                        true
                        (recur more (conj been pd)))))))]
    (-> (loop [[[pos dir] & more] (walk size obstacle start-pos :up) been #{} new-obstacle #{}]
          (if pos
            (let [next-pos (move dir pos)]
              (recur more (conj been [pos dir])
                     (cond-> new-obstacle
                       (loop? been next-pos pos dir) (conj next-pos))))
            new-obstacle))
        count))
  ; 1976
  )
