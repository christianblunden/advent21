(ns problem2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(comment
  (defn parse-line [line] (update (str/split line #" ") 1 read-string))
  (defn load-data [f] (->> f io/reader line-seq (map parse-line)))
  (def example-input (load-data "resources/input2eg.txt"))
  (def input (load-data "resources/input2.txt"))

  ;; part 1
  (defn mover [location [cmd val]]
    (update location
            ({"forward" :horizontal} cmd :depth)
            ({"up" -} cmd +)
            val))

  (defn solve [input]
    (->> input
         (reduce mover {:horizontal 0 :depth 0})
         vals
         (apply *)))
  
  (solve example-input) ; 150
  (solve input) ; 2039912

  ;; part 2
  (defmulti process-cmd (fn [_ [cmd _]] cmd))
  (defmethod process-cmd "forward" [{:keys [aim] :as location} [_ val]]
    (-> location
        (update :horizontal + val)
        (update :depth + (* aim val))))
  (defmethod process-cmd "up" [location [_ val]]
    (update location :aim - val))
  (defmethod process-cmd "down" [location [_ val]]
    (update location :aim + val))
  
  (defn solve2 [input]
    (->> input
      (reduce process-cmd {:horizontal 0 :depth 0 :aim 0})
      ((juxt :horizontal :depth))
      (apply *)))
  
  (solve2 example-input) ; 900
  (solve2 input) ; 1942068080

  )