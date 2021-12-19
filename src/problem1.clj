(ns problem1
  (:require [clojure.java.io :as io]))

(def input (->> "resources/input1.txt" io/reader line-seq (map read-string)))

;; part 1
(defn solve [input]
  (->> input
       (partition 2 1)
       (filter (fn [[x y]] (< x y)))
       count))

(solve input) ;1655

;; part 2
(->> input
     (partition 3 1)
     (map #(apply + %))
     solve) ; 1683

;; Matt phone bus
(defn depth-change 
  [[m & ms]]
  (loop [last m
         ms ms 
         cnt 0]
    (let [[x & xs] ms
          new-cnt (if (< last x) (inc cnt) cnt)]
      (if xs
        (recur x xs new-cnt)
        new-cnt))))

(depth-change input)
