(ns day09)

(def small-input "2333133121414131402")
(def input (slurp (format "src/%s.txt" *ns*)))

(comment

  (let [fds (->> input
                 (re-seq #"\d")
                 (into []
                       (comp (map parse-long)
                             (partitionv-all 2)
                             (map-indexed vector))))]
    (->> (range (count fds))
         (reduce (fn [[blks fds] id]
                   (if-some [[id [n free]] (get fds id)]
                     (loop [blks (into blks (repeat n id)) fds fds free free]
                       (if (pos? free)
                         (let [[m-id [n]] (last fds)]
                           (if (= m-id id)
                             [blks fds]
                             (recur (into blks (repeat (min n free) m-id))
                                    (if (>= free n)
                                      (subvec fds 0 m-id)
                                      (update-in fds [m-id 1 0] - free))
                                    (- free n))))
                         [blks fds]))
                     (reduced blks)))
                 [[] fds])
         (map-indexed *)
         (reduce +)))
  ; 6385338159127

  (let [fds (->> input
                 (re-seq #"\d")
                 (into []
                       (comp (map parse-long)
                             (partitionv-all 2)
                             (map-indexed (fn [id [n free]] [id (or free 0) (vec (repeat n id))])))))]
    (->> (reverse (next fds))
         (reduce (fn [fds [id _ blks]]
                   (let [n (count blks)]
                     (if-some [[o-id o-free] (first (drop-while #(< (second %) n) (take id fds)))]
                       (let [[_ free & [blks & more]] (get fds id)]
                         #_(clojure.pprint/pprint [id free blks more [o-id o-free]])
                         (cond-> (assoc fds id (into [id (cond-> free (not more) (+ n))] more))
                           :blks (update o-id (fnil conj []) blks)
                           :free (update-in [o-id 1] - n)
                           more (update-in [(dec id) 1] + n)))
                       fds)))
                 fds)
         (mapcat (fn [[_ free & blks]]
                   (-> (reduce into [] blks)
                       (into (repeat free 0)))))
         (map-indexed *)
         (reduce +)))
  ; 6415163624282
  )
