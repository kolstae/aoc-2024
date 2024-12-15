(ns day13
  (:require [clojure.string :as str]))

(def small-input "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n\nButton A: X+26, Y+66\nButton B: X+67, Y+21\nPrize: X=12748, Y=12176\n\nButton A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450\n\nButton A: X+69, Y+23\nButton B: X+27, Y+71\nPrize: X=18641, Y=10279")
(def input (slurp (format "src/%s.txt" *ns*)))

(defn parse-input [s]
  (->> (str/split-lines s)
       (filter seq)
       (mapv (fn [line] (mapv parse-long (re-seq #"\d+" line))))
       (partitionv 3)))

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


  (->> (parse-input input)
       (mapv #(update % 2 (partial mapv + (repeat 10000000000000))))
       (keep (fn [[[ax ay] [bx by] [px py]]]
               (let [cb (long (/ (- (* ax py) (* ay px)) (- (* ax by) (* ay bx))))
                     ca (long (/ (- px (* bx cb)) ax))]
                 (when (and (= (+ (* ca ax) (* cb bx)) px)
                            (= (+ (* ca ay) (* cb by)) py))
                   (+ (* 3 ca) cb)))))
       (reduce +))
  ; 104958599303720
  )
