(ns problem12
  (:require [clojure.string :as s]))

(defn parse-line [line](s/split line #"-"))
(defn load-data [f] (->> f slurp s/split-lines (map parse-line)))
(def example-input (load-data "resources/input12eg.txt"))
(def example-input2 (load-data "resources/input12eg2.txt"))
(def example-input3 (load-data "resources/input12eg3.txt"))
(def input (load-data "resources/input12.txt"))

(defn append [a b] (conj (or a []) b))
(defn build-graph [input]
  (reduce (fn [graph [k v]]
            (-> graph
                (update k append v)
                (update v append k))) {} input))

(defn small? [x] (= x (s/lower-case x)))

(defn small-cave-once [path options]
  (as-> path x
    (filter small? x)
    (set x)
    (remove x options)))

(defn explore
  [graph goal cave-strategy]
  (fn search
    [path]
    (let [current (peek path)]
      (if (= goal current)
        [path]
        (->> current
             graph
             (cave-strategy path)
             (mapcat #(search (conj path %))))))))

(defn solve [input strategy]
  (->> ["start"] 
   ((explore (build-graph input) "end" strategy))
      count))

(solve example-input small-cave-once) ; 10
(solve example-input2 small-cave-once) ; 19
(solve example-input3 small-cave-once) ; 226
(solve input small-cave-once) ; 3485

;; part 2
(defn visited-before? [n [k v]]
  (or (= k "start")
      (<= n v)))

(defn max-count [freq]
  (if-let [vals (vals freq)]
          ({2 1, 1 2} (apply max vals))
          2))

(defn small-cave-twice [path options]
  (let [freq (->> path (filter small?) frequencies)]
    (as-> freq x
      (filter (partial visited-before? (max-count freq)) x)
      (map first x)
      (set x)
      (remove x options))))

(solve example-input small-cave-twice) ; 36
(solve example-input2 small-cave-twice) ; 103
(solve example-input3 small-cave-twice) ; 3509
(solve input small-cave-twice) ; 85062
