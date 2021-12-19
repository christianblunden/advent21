(ns problem7
  (:require [clojure.core.matrix :as m]))

(defn load-data [f] (->> f slurp (re-seq #"\d+") (map read-string)))
(def example-input (load-data "resources/input7eg.txt"))
(def input (load-data "resources/input7.txt"))

(defn range-inc [x y] (range x (inc y)))

(defn crab-fuel [to crab]
  (m/esum (range-inc 1 (m/distance to crab))))

(defn solve [crabs fuel-fn]
  (let [[min-pos max-pos] ((juxt m/emin m/emax) crabs)]
    (->> (range-inc min-pos max-pos)
         (map #(m/esum (map (partial fuel-fn %) crabs)))
         m/emin)))

(time (solve example-input m/distance)) ; 37 "Elapsed time: 2.734479 msecs"
(time (solve input m/distance)) ; 345035 "Elapsed time: 804.632603 msecs"

(time (solve example-input crab-fuel)) ; 168 "Elapsed time: 1.014725 msecs"
(time (solve input crab-fuel)) ; 97038163 "Elapsed time: 397098.904856 msecs"