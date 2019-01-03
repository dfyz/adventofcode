(ns tasks.09
  (:require [clojure.data.finger-tree :as ft]))

(defn step-simple [nums idx next-marble]
  (let [split-pos (rem (inc idx) (count nums))
        idx-next (inc split-pos)
        [before middle after] (ft/ft-split-at nums split-pos)
        nums-next (ft/ft-concat (conj (conj before middle) next-marble) after)]
    [nums-next idx-next 0]))

(defn step-complex [nums idx next-marble]
  (let [to-remove-idx (rem
                        (+ (count nums) (- idx 7))
                        (count nums))
        [before removed after] (ft/ft-split-at nums to-remove-idx)
        nums-next (ft/ft-concat before after)
        idx-next (rem to-remove-idx (count nums-next))]
    [nums-next idx-next (+ removed next-marble)]))

(defn step [nums idx next-marble]
  (let [step-func (if (zero? (rem next-marble 23))
                    step-complex step-simple)]
    (step-func nums idx next-marble)))

(defn play [turn-count player-count]
  (let [scores (vec (repeat player-count 0))]
    (loop [marble 1
           scores scores
           nums (ft/counted-double-list 0)
           idx 0
           player-idx 0]
      (if (> marble turn-count)
        scores
        (let [[nums-next idx-next bonus] (step nums idx marble)
              score-next (+ bonus (get scores player-idx))]
          (recur
            (inc marble)
            (assoc scores player-idx score-next)
            nums-next
            idx-next
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