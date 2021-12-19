(ns problem9
  (:require [clojure.core.matrix :as m]))

(m/set-current-implementation :ndarray)

(defn load-data [f n] (->> f slurp (re-seq #"\d") (map read-string) (partition n) m/matrix))
(def example-input (load-data "resources/input9eg.txt" 10))
(def input (load-data "resources/input9.txt" 100))

;; part 1
(def offsets [[-1 0] [1 0] [0 -1] [0 1]])
(defn larger-neighbours [input]
  (->> offsets
       (map (comp (partial m/lt input)
                  (partial m/shift input)))
       (apply m/add)))

(defn risk [input]
  (let [counts (larger-neighbours input)
        [rows cols] (->> counts m/shape (m/emap dec))
        corners [[0 0] [0 cols] [rows 0] [rows cols]]]
    (m/emap-indexed (fn [[row col] x]
                      (let [c (m/mget counts row col)
                            required (cond
                                       (some #{[row col]} corners) 2
                                       (or (= row 0) (= col 0) (= row rows) (= col cols)) 3
                                       :else 4)]
                        (if (= c required) (inc x) 0))) input)))

(defn solve [input]
  (->> input
       risk
       m/esum))

(solve example-input) ; 15
(solve input) ; 498

;; part 2
(defn dfs [m start [rows cols] island visited]
  (loop [remaining [start]]
    (let [[row col] (first remaining)
          _ (m/mset! visited row col island)
          neighbours (map (partial m/add [row col]) offsets)
          valid (filter (fn [[i j]]
                          (and (<= 0 i rows)
                               (<= 0 j cols)
                               (not (zero? (m/mget m i j)))
                               (zero? (m/mget visited i j)))) neighbours)
          remaining (concat (rest remaining) valid)]
      (when-not (empty? remaining)
        (recur remaining)))))

(defn islands [input]
  (let [[rows cols] (m/shape input)
        visited (m/new-matrix rows cols)]
    (reduce (fn [count index]
              (if (and (< 0 (apply m/mget input index))
                       (zero? (apply m/mget visited index)))
                (do
                  (dfs input index [(dec rows) (dec cols)] count visited)
                  (inc count))
                count))
            1
            (m/index-seq input))
    visited))

(defn solve2 [input]
  (->> input
       larger-neighbours
       islands
       (m/ereduce (fn [freqs i] (if (< 0 i)
                                  (update freqs i #(inc (or % 0)))
                                  freqs)) {})
       ((comp reverse sort vals))
       (take 3)
       (apply *)))

(solve2 example-input) ; 1134
(solve2 input) ; 1029392