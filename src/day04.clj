(ns day04
  (:require [clojure.string :as str]))

(def small-input "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX")
(def small-input-2 ".M.S......\n..A..MSMS.\n.M.S.MAA..\n..A.ASMSM.\n.M.S.M....\n..........\nS.S.S.S.S.\n.A.A.A.A..\nM.M.M.M.M.\n..........")
(def input (slurp (format "src/%s.txt" *ns*)))

(defn count-xmas [s-or-coll]
  (let [s (cond->> s-or-coll
            (coll? s-or-coll) (str/join \n))]
    (+ (count (re-seq #"XMAS" s)) (count (re-seq #"SAMX" s)))))

(comment

  (let [#_#_input small-input
        h-lines (str/split-lines input)
        len (count h-lines)
        v-lines (mapv str/join (apply mapv (comp vec reverse vector) h-lines))
        ur (for [o (range (- (- len 4)) (- len 3))]
             (str/join (for [i (range len) :let [j (+ o i)] :when (< -1 j len)]
                         (get-in h-lines [j i]))))
        ul (for [o (range (- (- len 4)) (- len 3))]
             (str/join (for [i (range len) :let [j (+ o i)] :when (< -1 j len)]
                         (get-in v-lines [j i]))))]
    (->> [h-lines v-lines ur ul]
         (mapv count-xmas)
         (reduce +)))
  ; 2358

  (let [#_#_input small-input-2
        lines (str/split-lines input)
        len (count lines)]
    (->> (for [j (range 1 (dec len)) i (range 1 (dec len))
               :when (and (= \A (get-in lines [j i]))
                          (->> [[(dec j) (dec i)] [(inc j) (inc i)]]
                               (into #{} (map (partial get-in lines)))
                               (= #{\M \S}))
                          (->> [[(dec j) (inc i)] [(inc j) (dec i)]]
                               (into #{} (map (partial get-in lines)))
                               (= #{\M \S})))]
           [j i ])
         count))
  ; 1737
  )
