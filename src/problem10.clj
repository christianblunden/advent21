(ns problem10
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn parse-line [line]
  (->> line (re-seq #"[()\[\]{}<>]")))
(defn load-data [f] (->> f slurp
                         s/split-lines
                         (map parse-line)))
(def example-input (load-data "resources/input10eg.txt"))
(def input (load-data "resources/input10.txt"))

(def scores1 {")" 3
              "]" 57
              "}" 1197
              ">" 25137})
(def open? #{"{" "[" "<" "("})
(def openers {"}" "{"
              "]" "["
              ">" "<"
              ")" "("})
(def closers (set/map-invert openers))

(defn parse [line]
  (reduce (fn [acc chunk]
            (if (open? chunk)
              (conj acc chunk)
              (if (= (openers chunk) (first acc))
                (rest acc)
                (reduced {:expected (first acc) :got chunk}))))
          '()
          line))

;; part 1
(defn solve [input]
  (->> input
       (map parse)
       (filter map?)
       (map (comp scores1 :got))
       (apply +)))

(solve example-input) ; 26397
(solve input) ; 394647

;; part 2
(def scores2 {")" 1
              "]" 2
              "}" 3
              ">" 4})

(defn score [incomplete]
  (->> incomplete
       (map closers)
       (reduce #(+ (scores2 %2) (* % 5)) 0)))

(defn auto-complete-score [scores]
  (->> scores
       sort
       (drop (quot (count scores) 2))
       first))

(defn solve2 [input]
  (->> input
       (map parse)
       (filter list?)
       (map score)
       auto-complete-score))

(solve2 example-input) ; 288957
(solve2 input) ; 2380061249