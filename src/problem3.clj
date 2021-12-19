(ns problem3
  (:require [clojure.java.io :as io]
            [clojure.core.matrix :as m]))

(comment

  (def input (->> "resources/input3.txt" io/reader line-seq (map #(->> % (re-seq #"\d") (into [])))))
  (def example-input (->> "resources/input3eg.txt" io/reader line-seq (map #(->> % (re-seq #"\d") (into [])))))

  ;; part 1
  (def flipr {\0 "1" \1 "0"})
  (defn to-decimal [s] (Integer/parseInt s 2))

  (defn most-common [col]
    (-> col frequencies (max-key "0" "1")))

  (defn gamma [input]
    (->> input
         m/columns
         (map most-common)
         (apply str)))

  (defn solve [input]
    (let [gam (gamma input)
          eps (->> gam (map flipr) (apply str))]
      (* (to-decimal gam) (to-decimal eps))))

  (solve example-input) ;198
  (solve input) ; 3958484

  ;; part 2
  (defn least-common [col]
    (-> col frequencies (min-key "0" "1")))

  (defn life-support [input bit-criteria]
    (loop [m input
           col 0]
      (let [filter-bit (-> m m/columns (get col) bit-criteria)
            results (filter #(= (nth % col) filter-bit) m)]
        (if (= 1 (count results))
          (->> results first (apply str) to-decimal)
          (recur results (inc col))))))
  
  (defn solve2 [input]
    (let [co2 (life-support input most-common)
          oxygen (life-support input least-common)]
      (* co2 oxygen)))
  
  (solve2 example-input) ; 230
  (solve2 input) ; 1613181

  )