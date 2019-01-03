(ns tasks.09
  (:require [clojure.data.finger-tree :as ft]))

(defn rotate-left [nums]
  (conj (rest nums) (first nums)))

(defn rotate-right [nums]
  (ft/conjl (pop nums) (peek nums)))

(defn step-simple [nums next-marble]
  (let [nums-next (ft/conjl (rotate-left (rotate-left nums)) next-marble)]
    [nums-next 0]))

(defn step-complex [nums next-marble]
  (let [rotated (nth (iterate rotate-right nums) 7)]
    [(rest rotated) (+ next-marble (first rotated))]))

(defn step [nums next-marble]
  (let [step-func (if (zero? (rem next-marble 23))
                    step-complex step-simple)]
    (step-func nums next-marble)))

(defn play [turn-count player-count]
  (let [scores (vec (repeat player-count 0))]
    (loop [marble 1
           scores scores
           nums (ft/counted-double-list 0)
           player-idx 0]
      (if (> marble turn-count)
        scores
        (let [[nums-next bonus] (step nums marble)
              score-next (+ bonus (get scores player-idx))]
          (recur
            (inc marble)
            (assoc scores player-idx score-next)
            nums-next
            (rem (inc player-idx) (count scores))))))))

(def sample [[25 9] [1618 10] [7999 13] [1104 17] [6111 21] [5807 30]])
(def input
  (let [content (slurp "inputs\\09.txt")
        [_ player-count turn-count] (re-find #"(\d+).*;.*?(\d+)" content)]
    [(Integer/parseInt turn-count) (Integer/parseInt player-count)]))

(defn solve-easy [[turn-count player-count]]
  (apply max (play turn-count player-count)))

(defn solve-hard [[turn-count player-count]]
  (solve-easy [(* 100 turn-count) player-count]))