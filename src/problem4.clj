(ns problem4
  (:require [clojure.java.io :as io]
            [clojure.core.matrix :as m]))

(defn parse-ints [x] (->> x (re-seq #"\d+") (map read-string)))
(defn load-data [f] (->> f io/reader line-seq (map parse-ints)))
(def example-input (load-data "resources/input4eg.txt"))
(def input (load-data "resources/input4.txt"))

;; part 1
(defn parse-boards [data]
  (->> data
       (remove empty?)
       (partition 5)
       (map m/matrix)))

(defn win? [draw board]
  (or (->> board (map #(every? draw %)) (some true?))
      (->> board m/columns (map #(every? draw %)) (some true?))))

(defn score [{draw :draw [board & _] :winners}]
  (->> board
       flatten
       (remove (set draw))
       (apply +)
       (* (last draw))))

(defn bingo-rounds [[draw-data & board-data]]
  (let [boards (parse-boards board-data)]
    (reductions
     (fn [{:keys [draw round losers]} next]
       (let [draw (conj draw next)
             winners (filter #(win? (set draw) %) losers)]
         {:winners winners :draw draw :round (inc round) :losers (remove #(win? (set draw) %) losers)}))
     {:round 0 :draw [] :losers boards}
     draw-data)))

(defn play [input strategy]
  (->> (bingo-rounds input)
       (drop-while strategy)
       first
       score))

(def winning-strategy (comp empty? :winners))

(play example-input winning-strategy) ; 4512
(play input winning-strategy) ; 74320

;; part 2

(def losing-strategy (comp not empty? :losers))

(play example-input losing-strategy) ; 1924
(play input losing-strategy) ; 17884
