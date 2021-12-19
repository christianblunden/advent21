(ns problem5
  (:require [clojure.java.io :as io]
            [clojure.core.matrix :as m]))

(defn parse-line [line] (->> line (re-seq #"\d+") (map read-string) (partition 2) (map m/matrix)))
(defn load-data [f] (->> f io/reader line-seq (map parse-line)))
(def example-input (load-data "resources/input5eg.txt"))
(def input (load-data "resources/input5.txt"))

(m/set-current-implementation :ndarray)

;; part 1
(defn linear? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

;; part2
(defn diagonal? [[[x1 y1] [x2 y2]]]
  (= (m/distance x1 x2) (m/distance y1 y2)))

(defn fillr [[[x1 y1] [x2 y2] :as jet]]
  (let [rows (inc (m/distance y1 y2))
        cols (inc (m/distance x1 x2))]
    (if (diagonal? jet)
      (cond->> (m/identity-matrix rows cols)
        (and (> x1 x2) (< y1 y2)) reverse
        (and (< x1 x2) (> y1 y2)) reverse)
      (->> (m/new-matrix rows cols)
           (m/emap inc)))))

(defn max-dimensions [input]
  (->> input
       (mapcat concat)
       m/columns
       (mapv (comp inc m/emax))
       reverse))

(defn vent [max-dims [[x1 y1] [x2 y2] :as jet]]
  (let [grid (apply m/new-matrix max-dims)
        fill (fillr jet)
        [rows cols] (m/shape fill)]
    (m/assign! (m/submatrix grid [[(min y1 y2) rows] [(min x1 x2) cols]]) (fillr jet))
    grid))

(defn solve [input]
  (let [max-dims (max-dimensions input)]
    (as-> input x
      (map (partial vent max-dims) x)
      (apply m/add x)
      (m/ge x 2)
      (m/esum x))))

(solve (filter linear? example-input)) ; 5
(solve (filter linear? input)) ; 6710

;; part 2

(solve example-input) ; 12
(solve input) ; 20121