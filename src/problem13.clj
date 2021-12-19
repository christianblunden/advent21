(ns problem13
  (:require [clojure.java.io :as io]
            [clojure.core.matrix :as m]
            [clojure.string :as s]))

(defn parse-coord [line] (->> line (re-seq #"\d+") (mapv read-string)))
(defn parse-fold [line] (->> line 
                             (re-matches #"fold along ([xy])=(\d+)")
                             rest
                             (map read-string)
                             vec))
(defn fold? [line] (s/starts-with? line "fold along"))
(defn load-data [f]
  (let [[coords folds] (->> f io/reader line-seq (remove s/blank?) (split-with (complement fold?)))]
    {:coords (map parse-coord coords)
     :folds (map parse-fold folds)}))

(def example-input (load-data "resources/input13eg.txt"))
(def input (load-data "resources/input13.txt"))

(m/set-current-implementation :ndarray)

(defn build-paper [{:keys [coords]}]
  (let [cols (apply max (map first coords))
        rows (apply max (map second coords))
        m (m/new-matrix (inc rows) (inc cols))]
    (doseq [[x y] coords]
      (m/mset! m y x 1))
    m))

(defn shrink [fold-at folded]
  (m/reshape folded (assoc (m/shape folded) 0 fold-at)))

(defn fold [paper [axis fold-at]]
  (let [p (if (= axis 'x) (m/transpose paper) paper)]
    (cond->> (range 0 (inc fold-at))
      true (reduce #(m/add-row % (- fold-at %2) (+ fold-at %2)) p)
      true (shrink fold-at)
      (= axis 'x) m/transpose)))

(defn solve [{:keys [folds] :as input} n]
  (let [paper (build-paper input)]
    (as-> folds x
      (reductions fold paper x)
      (take (inc n) x)
      (last x)
      (m/ge x 1)
      (m/esum x))))

(solve example-input 1) ; 17
(solve input 1) ; 747

(defn solve2 [{:keys [folds] :as input}]
  (let [paper (build-paper input)]
    (as-> folds x
      (reductions fold paper x)
      (last x)
      (m/ge x 1)
      (m/pm x))))

(solve2 example-input) ; "0"
(solve2 input) ; "ARHZPCUH"