(ns problem6)

(defn load-data [f] (->> f slurp (re-seq #"\d") (map read-string)))
(def example-input (load-data "resources/input6eg.txt"))
(def input (load-data "resources/input6.txt"))

(defn simulate2 [[ripe & fish] _]
  (-> fish vec (update 6 + ripe) (conj ripe)))

(defn pack [input]
  (reduce #(update %1 %2 inc) (->> (repeat 0) (take 9) vec) input))

(defn solve2 [days input]
  (->> (range days)
       (reduce simulate2 (pack input))
       (apply +)))

(time (solve2 80 example-input)) ; 5934
(time (solve2 80 input)) ; 362639
;; part 2
(time (solve2 256 example-input)) ; 26984457539
(time (solve2 256 input)) ; 1639854996917 "Elapsed time: 0.495618 msecs"