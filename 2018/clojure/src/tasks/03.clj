(ns tasks.03
  (:require [clojure.set]))

(defn parse-claim [s]
  (let [groups (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s)
        [num x y w h] (map #(Integer/parseInt %) (drop 1 groups))]
    {:num num :x x :y y :w w :h h}))

(def sample (map parse-claim ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"]))
(def input (map parse-claim (clojure.string/split-lines (slurp "inputs/03.txt"))))

(defn to-cells [claim]
  (letfn [(to-range [x len] (range x (+ x len)))]
    (for [x (to-range (claim :x) (claim :w))
          y (to-range (claim :y) (claim :h))]
      [x y])))

(defn all-cells [claims]
  (->> claims
       (map to-cells)
       (apply concat)))

(defn get-dups [claims]
  (->> claims
       all-cells
       frequencies
       (filter #(> (val %) 1))
       (map key)
       set))

(defn dup-set [input] (get-dups input))

(defn solve-easy [input]
  (count (dup-set input)))

(defn solve-hard [input]
  (let [dups (dup-set input)]
    (->> input
         (filter #(empty? (clojure.set/intersection dups (set (to-cells %)))))
         first
         :num)))
