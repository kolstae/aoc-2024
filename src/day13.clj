(ns day13
  (:require [clojure.string :as str]))

(def small-input "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n\nButton A: X+26, Y+66\nButton B: X+67, Y+21\nPrize: X=12748, Y=12176\n\nButton A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450\n\nButton A: X+69, Y+23\nButton B: X+27, Y+71\nPrize: X=18641, Y=10279")
(def input (slurp (format "src/%s.txt" *ns*)))

(defn parse-input [s]
  (->> (str/split-lines s)
       (filter seq)
       (mapv (fn [line] (mapv parse-long (re-seq #"\d+" line))))
       (partitionv 3)))

(defn gdc [^long a ^long b]
  (if (= a b)
    a
    (if (< a b)
      (recur (- b a) a)
      (recur (- a b) a))))

(defn lcm [^long a ^long b]
  (* a (/ b (gdc a b))))

(comment

  (->> (parse-input input)
       (keep (fn [[a [bx by] d]]
               (->> d
                    (iterate (fn [pd] (mapv - pd a)))
                    (take-while #(not-any? neg? %))
                    (map-indexed (fn [i [dx dy]]
                                   [(+ (/ dx bx) (* i 3))
                                    (and (zero? (mod dx bx))
                                         (zero? (mod dy by))
                                         (= (/ dx bx) (/ dy by)))]))
                    (filter second)
                    ffirst)))
       (reduce +))
  ; 29438



  (->> (parse-input small-input)
       (take 1)
       (keep (fn [[[ax ay] [bx by] d]]
               (let [[dx dy] d #_(mapv * d (repeat 10000000000000))
                     lcm-x (lcm ax bx)
                     lcm-y (lcm ay by)]
                 [lcm-x lcm-y (gdc lcm-x lcm-y) d
                  [(/ lcm-x ax) (/ lcm-y ay)]
                  [(* 80 ax) (* 80 ay)]])
               #_(->> d
                    (iterate (fn [pd] (mapv - pd a)))
                    (take-while #(not-any? neg? %))
                    (map-indexed (fn [i [dx dy]]
                                   [(+ (/ dx bx) (* i 3))
                                    (and (zero? (mod dx bx))
                                         (zero? (mod dy by))
                                         (= (/ dx bx) (/ dy by)))]))
                    (filter second)
                    ffirst)))
       #_(reduce +))
  ;
  )
