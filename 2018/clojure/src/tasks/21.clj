(ns tasks.21
  (:require [tasks.vm-utils]))

(def input (tasks.vm-utils/parse-input "inputs/21.txt"))

(defn rng-step [r1]
  (let [r4 (bit-or r1 65536)]
    (loop [r4 r4
           r1 3798839]
      (if (= r4 0)
        r1
        (recur
          (quot r4 256)
          (let [r4-low (bit-and r4 255)
                trunc #(bit-and % 16777215)]
            (trunc (* 65899 (trunc (+ r1 r4-low))))))))))

(defn solve-easy [_] (rng-step 0))

(defn solve-hard [_]
  (loop [cur 0 used #{}]
    (let [next (rng-step cur)]
      (if (contains? used next)
        cur
        (recur next (conj used cur))))))
