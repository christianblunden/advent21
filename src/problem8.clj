(ns problem8
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn parse-line [line]
  (->> line
       (re-seq #"[abcdefg]+")
       (map set)))
(defn load-data [f] (->> f slurp
                         s/split-lines
                         (map parse-line)
                         (map #(partition-all 10 %))))
(def example-input (load-data "resources/input8eg.txt"))
(def input (load-data "resources/input8.txt"))

(def segmentz ["abcefg"
               "cf"
               "acdeg"
               "acdfg"
               "bcdf"
               "abdfg"
               "abdefg"
               "acf"
               "abcdefg"
               "abcdfg"])
(def seg->count (mapv count segmentz))

(defn parse-display [acc [_ output]]
  (->> output
       (map count)
       (map #(.indexOf seg->count %))
       frequencies
       (merge-with + acc)))

(defn solve [input numbers-to-add]
  (let [counts (reduce parse-display {} input)]
    (reduce + (map counts numbers-to-add))))

(solve example-input [1 4 7 8]) ; 26
(solve input [1 4 7 8]) ; 543

;; part 2
(defn length= [n sig]
  (= n (count sig)))

(defn contains= [sig1 sig2]
  (= (set/intersection sig1 sig2) sig2))

(defn combine= [sig1 sig2 sig3]
  (= (set/union sig1 sig2) sig3))

(def parsers {0 (fn [mappings signal] (and (length= 6 signal) (not= signal (mappings 6)) (not= signal (mappings 9))))
              1 (fn [_ signal] (length= 2 signal))
              2 (fn [mappings signal] (and (length= 5 signal) (not= signal (mappings 3)) (not= signal (mappings 5))))
              3 (fn [mappings signal] (and (length= 5 signal) (contains= signal (mappings 1))))
              4 (fn [_ signal] (length= 4 signal))
              5 (fn [mappings signal] (and (length= 5 signal) (combine= signal (mappings 1) (mappings 9))))
              6 (fn [mappings signal] (and (length= 6 signal) (not (contains= signal (mappings 1)))))
              7 (fn [_ signal] (length= 3 signal))
              8 (fn [_ signal] (length= 7 signal))
              9 (fn [mappings signal] (and (length= 6 signal) (contains= signal (mappings 3))))})

(defn parse-signals [signals]
  (loop [ss signals
         [p & ps] [1 4 7 8 3 9 6 0 5 2]
         mappings {}]
    (if (empty? ss)
      mappings
      (let [[match] (filter (partial (parsers p) mappings) ss)
            rem (remove (partial (parsers p) mappings) ss)]
        (recur rem ps (assoc mappings p match))))))

(defn parse-digits [[signals digits]]
  (-> signals
      parse-signals
      set/map-invert
      (map digits)))

(defn solve2 [input]
  (->> input
       (map parse-digits)
       (map #(Integer/parseInt (apply str %)))
       (reduce +)))

(solve2 example-input) ; 61229
(solve2 input) ; 994266
