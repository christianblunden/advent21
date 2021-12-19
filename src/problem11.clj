(ns problem11
  (:require [clojure.core.matrix :as m]))

(m/set-current-implementation :ndarray)

(defn load-data [f] (->> f slurp (re-seq #"\d") (map read-string) (partition 10) m/matrix))
(def example-input (load-data "resources/input11eg.txt"))
(def input (load-data "resources/input11.txt"))

(def offsets (for [i [-1 0 1] j [-1 0 1] :when (not= i j 0)] [i j]))
(def ones (m/emap inc (m/zero-matrix 10 10)))

(defn radiate [octopus]
  (loop [o octopus
         flashed (m/zero-matrix 10 10)]
    (let [flashing (m/sub (m/gt o 9) flashed)
          radiation (->> offsets
                         (map (partial m/shift flashing))
                         (apply m/add))]
      (if (< 0 (m/esum flashing))
        (recur (m/add o radiation) (m/add flashed flashing))
        o))))

(defn cycle [{:keys [flashes octopus]} i]
  (let [levelup (m/add ones octopus)
        new-total (radiate levelup)]
    {:flashes (+ flashes (m/esum (m/gt new-total 9)))
     :octopus (m/e* (m/le new-total 9) new-total)}))

(defn solve [input n]
  (->> (range n)
       (reduce cycle {:flashes 0 :octopus input})
       :flashes))

(solve example-input 100) ; 1656
(solve input 100) ; 1669

(defn solve2 [input]
  (->> (range)
       (reductions cycle {:flashes 0 :octopus input})
       (map (comp m/maximum :octopus))
       (take-while #(< 0 %))
       count))

(solve2 example-input) ; 195
(solve2 input) ; 195