(ns tasks.25
  (:require [clojure.set]))

(defn parse-nums [tokens]
  (vec (map #(Integer/parseInt %) tokens)))

(defn parse-input [filename]
  (vec (map
         #(parse-nums (clojure.string/split % #","))
         (clojure.string/split-lines (slurp filename)))))

(def samples (vector
               (parse-input "inputs/25_sample1.txt")
               (parse-input "inputs/25_sample2.txt")
               (parse-input "inputs/25_sample3.txt")
               (parse-input "inputs/25_sample4.txt")))
(def input (parse-input "inputs/25.txt"))

(defn connected [[x1 y1 z1 t1] [x2 y2 z2 t2]]
  (let [d (fn [c1 c2] (Math/abs (- c1 c2)))]
    (<=
      (+ (d x1 x2) (d y1 y2) (d z1 z2) (d t1 t2))
      3)))

(defn to-graph [input]
  (vec
    (for [from (range (count input))]
      (vec
        (for [to (range (count input))
              :when (not= from to)
              :when (connected (get input from) (get input to))]
          to)))))

(defn find-constellation [start members graph]
  (loop [members (conj members start)
         adj (filter (comp not members) (get graph start))]
    (if (empty? adj)
      members
      (let [[x & xs] adj
            members (if (contains? members x)
                      members
                      (clojure.set/union members
                                         (find-constellation x (conj members x) graph)))]
        (recur members xs)))))

(defn solve-easy [input]
  (let [graph (to-graph input)]
    (loop [left (set (range (count graph)))
           constellation-count 0]
      (do
        (if (empty? left)
          constellation-count
          (recur
            (clojure.set/difference left (find-constellation (first left) #{} graph))
            (inc constellation-count)))))))
