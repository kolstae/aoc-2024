(ns day12
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def small-input "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE")
(def input (slurp (format "src/%s.txt" *ns*)))

(defn neighbours [p]
  (->> [[0 1] [1 0] [-1 0] [0 -1]]
       (mapv (partial mapv +) (repeat p))))

(defn neighbours-2 [p]
  (->> (neighbours p)
       (mapv (fn [side np]
               [np
                [side (case side
                        (:lower :upper) (vec (reverse np))
                        (:right :left) np)]])
             [:lower :right :left :upper])))

(comment

  (let [groups (->> (str/split-lines input)
                    (mapcat (fn [y l] (keep-indexed (fn [x c] [[x y] c]) l)) (range))
                    (group-by second)
                    vals
                    (mapv (partial into #{} (map first))))
        price (fn [ps] (* (count (into [] (comp (mapcat neighbours) (remove ps)) ps))
                          (count ps)))]
    (->> groups
         (mapcat (fn [group]
                   (loop [[p & ps] group areas []]
                     (if p
                       (let [n-ps (neighbours p)
                             {matching true others false} (group-by (fn [area]
                                                                      (boolean (some area n-ps))) areas)]
                         (recur ps (conj others (apply set/union #{p} matching))))
                       areas))))
         (mapv price)
         (reduce +)))
  ; 1319878


  (let [groups (->> (str/split-lines input #_"AAAA\nBBCD\nBBCC\nEEEC")
                    (mapcat (fn [y l] (keep-indexed (fn [x c] [[x y] c]) l)) (range))
                    (group-by second)
                    vals
                    (mapv (partial into #{} (map first))))
        price (fn [ps]
                (let [sides (->> ps
                                 (into [] (comp (mapcat neighbours-2) (remove (comp ps first)) (map second)))
                                 sort
                                 (partition-by (juxt first (comp first second)))
                                 (mapcat #(reduce (fn [ns [_ [_ n]]]
                                                  (if (= (peek ns) (dec n))
                                                    (conj (pop ns) n)
                                                    (conj ns n)))
                                                [] %)))]
                  #_(clojure.pprint/pprint [sides (->> ps
                                                     (into [] (comp (mapcat neighbours-2) (remove (comp ps first)) (map second)))
                                                     sort
                                                     (partition-by first))])
                  (* (count sides)
                     (count ps))))]
    (->> groups
         (mapcat (fn [group]
                   (loop [[p & ps] group areas []]
                     (if p
                       (let [n-ps (neighbours p)
                             {matching true others false} (group-by (fn [area]
                                                                      (boolean (some area n-ps))) areas)]
                         (recur ps (conj others (apply set/union #{p} matching))))
                       areas)))
                 )
         (mapv price)
         (reduce +)))
  ; 784982
  )
