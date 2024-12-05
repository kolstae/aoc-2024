(ns day05
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def small-input "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47")
(def input (slurp (format "src/%s.txt" *ns*)))

(defn split-ns [s]
  (mapv parse-long (str/split s #"[,|]")))

(defn parse-input [input]
  (update (->> (str/split-lines input)
               (split-with seq)
               (mapv (partial mapv split-ns)))
          1 subvec 1))                                      ; Remove the blank line

(defn correct-order? [before->after ns]
  (loop [seen #{} [n & more] ns]
    (if n
      (if-some [after (before->after n)]
        (when-not (seq (set/intersection seen after))
          (recur (conj seen n) more))
        (recur (conj seen n) more))
      true)))

(comment

  (let [[ordering updates] (parse-input input)
        before->after (-> (group-by first ordering)
                          (update-vals (partial into #{} (map second))))]
    (->> updates
         (filterv (partial correct-order? before->after))
         (mapv (fn [ns] (nth ns (/ (count ns) 2))))
         (reduce +)))
  ; 6051

  (let [[ordering updates] (parse-input input)
        before->after (-> (group-by first ordering)
                          (update-vals (partial into #{} (map second))))]
    (->> updates
         (remove (partial correct-order? before->after))
         (mapv (fn [ns]
                 (-> (sort-by (comp - count second)
                              (-> (select-keys before->after ns)
                                  (update-vals #(set/intersection % (set ns)))))
                     (nth (/ (count ns) 2))
                     first)))
         (reduce +)))
  ; 5093
  )
