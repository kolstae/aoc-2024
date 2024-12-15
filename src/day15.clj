(ns day15
  (:require [clojure.string :as str]))

(def small-input "##########\n#..O..O.O#\n#......O.#\n#.OO..O.O#\n#..O@..O.#\n#O#..O...#\n#O..O..O.#\n#.OO.O.OO#\n#....O...#\n##########\n\n<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\nvvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\nv^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")
(def x-small-input "########\n#..O.O.#\n##@.O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########\n\n<^^>>>vv<v>>v<<")
(def input (slurp (format "src/%s.txt" *ns*)))

(defn s->pos-c [ss]
  (->> ss
       (mapcat (fn [y l] (keep-indexed (fn [x c] (when (not= c \.) [[y x] c])) l))
               (range))
       (into {})))

(defn parse-input [s]
  (let [[wh [_ & instr]] (split-with seq (str/split-lines s))
        ps (s->pos-c wh)
        start-pos (ffirst (filter (comp #{\@} val) ps))]
    [start-pos (dissoc ps start-pos) (str/join instr)]))

(defn render [ps]
  (str/join \newline
            (let [max-x (range (inc (reduce max (map (comp second first) ps))))]
              (for [y (range (inc (reduce max (map ffirst ps))))]
                (str/join (for [x max-x]
                            (get ps [y x] \space)))))))

(defn wall? [ps moved-ps]
  (some (comp #{\#} ps) moved-ps))

(defn instr->delta [instr]
  (case instr
    \v [1 0]
    \^ [-1 0]
    \> [0 1]
    \< [0 -1]))

(comment

  (let [[start-pos ps instr] (parse-input input)]
    (loop [pos start-pos ps ps [instr & more] instr]
      #_(println (render (assoc ps pos \@)))
      (if instr
        (let [d (instr->delta instr)
              next-pos (mapv + d pos)
              moved-ps (take-while (comp #{\O \#} ps) (iterate (partial mapv + d) next-pos))]
          #_(clojure.pprint/pprint [pos instr next-pos moved-ps])
          (if (wall? ps moved-ps)
            (recur pos ps more)
            (recur next-pos
                   (reduce (fn [m from] (-> m (dissoc from) (assoc (mapv + d from) (get m from)))) ps (reverse moved-ps))
                   more)))
        (->> (filterv (comp #{\O} val) ps)
             (mapv (fn [[[y x]]] (+ (* 100 y) x)))
             (reduce +)))))
  ; 1538871


  (let [[start-pos ps instr] (parse-input input)
        start-pos (update start-pos 1 * 2)
        ps (into {}
                 (mapcat (fn [[[y x] c]]
                           [[[y (* 2 x)] (if (= c \O) \[ c)]
                            [[y (inc (* 2 x))] (if (= c \O) \] c)]]))
                 ps)]
    (loop [pos start-pos ps ps [instr & more] instr]
      #_(println (render (assoc ps pos \@)))
      (if instr
        (let [d (instr->delta instr)
              next-pos (mapv + d pos)
              moved-ps (cond->> (into [] (take-while (comp #{\[ \] \#} ps)) (iterate (partial mapv + d) next-pos))
                         (#{\v \^} instr) ((fn [done moved-ps]
                                             (if (wall? ps moved-ps)
                                               moved-ps
                                               (if-some [halves (->> (remove done moved-ps)
                                                                     (keep (fn [p] (case (ps p)
                                                                                     \[ (update p 1 inc)
                                                                                     \] (update p 1 dec))))
                                                                     not-empty)]
                                                 (recur (into done (concat moved-ps halves))
                                                        (into moved-ps
                                                              (distinct)
                                                              (mapcat #(take-while (comp #{\[ \] \#} ps) (iterate (partial mapv + d) %)) halves)))
                                                 moved-ps
                                                 #_(when (seq moved-ps)
                                                     (def state {:ps ps :pos pos :next-pos next-pos :instr instr :moved-ps moved-ps})
                                                     (throw (ex-info "" {}))))))
                                           #{}))]
          #_(clojure.pprint/pprint [pos instr next-pos moved-ps])
          (if (wall? ps moved-ps)
            (recur pos ps more)
            (recur next-pos (reduce (fn [m from] (-> m (dissoc from) (assoc (mapv + d from) (get m from)))) ps (reverse moved-ps)) more)))
        (->> (filterv (comp #{\[ } val) ps)
             (mapv (fn [[[y x]]] (+ (* 100 y) x)))
             (reduce +)))))
  ; 1543338

  (let [{:keys [ps pos next-pos instr moved-ps]} state
        done #{}
        d (instr->delta instr)]
    (println (render (assoc ps pos instr)))
    (->> (remove done moved-ps)
         (keep (fn [p] (case (ps p)
                         \[ (update p 1 inc)
                         \] (update p 1 dec))))
         (mapcat #(take-while (comp #{\[ \] \#} ps) (iterate (partial mapv + d) %)))
         distinct))
  )
