(ns problem14
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]
            [clojure.core.matrix :as m]))

(m/set-current-implementation :ndarray)

(defn parse-insertions [line] (->> line (re-seq #"[A-Z]") (apply str) vec))
(defn load-data [f]
  (let [[template insertions] (->> f io/reader line-seq (split-with (complement s/blank?)))]
    {:template (first template)
     :insertions (->> insertions (remove s/blank?) (map parse-insertions))}))

(def example-input (load-data "resources/input14eg.txt"))
(def input (load-data "resources/input14.txt"))

(def ones (m/fill (m/new-vector 26) 1))
(def idx->char (into {} (map-indexed vector "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
(def char->idx (set/map-invert idx->char))

(defn build-matrix [pairs]
  (->> pairs
       (map (fn [[x y v]] [(char->idx x) (char->idx y) (when v (char->idx v))]))
       (reduce (fn [m [x y v]] (m/mset m x y (or v (inc (m/mget m x y))))) 
               (m/new-matrix 26 26))))

(defn perform-insertions [insertions polymer _]
  (->> (m/index-seq-for-shape [26 26])
       (map (fn [[x y]]
              (let [insertion (m/mget insertions x y)
                    val (m/mget polymer x y)]
                (m/set-indices (m/new-matrix 26 26) [[x insertion] [insertion y]] val))))
       (apply m/add)))

;; (defn enhanced-score [polymer]
;;   (let [sums (mapv max (m/dot polymer ones) (m/dot ones polymer))
;;         length (inc (m/esum polymer))
;;         maxchar (idx->char (.indexOf sums max))
;;         minchar (idx->char (.indexOf sums min))
;;         max (m/emax sums)
;;         min (m/emin (remove zero? sums))]
;;     {:score (long (- max min)) :length (long length) :min (long min) :minchar minchar :max (long max) :maxchar maxchar}))

(defn score [polymer]
  ;; using a matrix to store the pairs of letters means that the length of the polymer
  ;; is the number of pairs + 1 (as the letters overlap)
  ;; each row represents the total of letters in the first position '[N]N' '[B]C'
  ;; each col represents the total of letters in the second position 'N[N] 'B[C]'
  ;; Due to overalpping of the letters only 1 total of rows or cols is required
  ;; one of these totals can be higher as the polymer can start and end on a letter, so won't appear
  ;; in another pair eg: '[N]NBC[H]' first N will appear in a column total but not row total
  ;; and vice versa for the ending H
  ;; dot product with a ones vector will sum the rows or columns depending on order it is used
  (let [sums (mapv max (m/dot polymer ones) (m/dot ones polymer))]
    (long (- (m/emax sums)
             (m/emin (remove zero? sums))))))

(defn solve [{:keys [template insertions]} n]
  (let [insertions (->> insertions build-matrix)]
    (->> (range n)
         (reduce
          (partial perform-insertions insertions)
          (->> template (partition 2 1) build-matrix))
         score)))

(solve example-input 10) ; 1588
(solve input 10) ; 2068

;; part 2
(solve example-input 40) ; 2188189693529
(time (solve input 40)) ; 2158894777814 "Elapsed time: 656.640191 msecs"