(ns problem14strings
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-insertions [line] (->> line (re-seq #"[A-Z]+") vec))
(defn load-data [f]
  (let [[template insertions] (->> f io/reader line-seq (split-with (complement s/blank?)))]
    {:template (first template)
     :insertions (->> insertions (remove s/blank?) (map parse-insertions) (into {}))}))

(def example-input (load-data "resources/input14eg.txt"))
(def input (load-data "resources/input14.txt"))

(defn step [insertions polymer _]
  (->> polymer
       (partition 2 1)
       (map (fn [[l r]]
               (str (insertions (str l r)) r)))
       (apply str (first polymer))))

(defn score [polymer]
  (let [counts (->> polymer frequencies)
        max (apply max (vals counts))
        max-char (apply max-key val counts)
        min (apply min (vals counts))
        min-char (apply min-key val counts)
        score (- max min)]
    {:score score :length (count polymer) :max max :maxchar max-char :min min :minchar min-char :freq counts}))

(defn solve [{:keys [template insertions]} n]
  (->> (range n)
       (reductions (partial step insertions) template)
       (map score)
       (map prn)))

(solve example-input 10) ; 1588
(solve input 10) ; 2068

;; (solve example-input 40) ; 1588 ; does not return
;; (solve input 40) ; 2068
