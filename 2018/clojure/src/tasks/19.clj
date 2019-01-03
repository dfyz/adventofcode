(ns tasks.19
  (:require [tasks.vm-utils]))

(def sample (tasks.vm-utils/parse-input "inputs\\19_sample.txt"))
(def input (tasks.vm-utils/parse-input "inputs\\19.txt"))

(defn simulate [input start-regs max-iter]
  (loop [ip 0
         regs start-regs
         iter 0]
    (if (and (not (nil? max-iter)) (> iter max-iter))
      regs
      (if (nil? (get (first input) ip))
        (get regs 0)
        (let [[ip-next regs-next] (tasks.vm-utils/step input ip regs)]
          (recur ip-next regs-next (inc iter)))))))

(defn solve-easy [input] (simulate input (vec (repeat 6 0)) nil))

(defn solve-hard [input]
  (let [target-r2 (get (simulate input [1 0 0 0 0 0] 100) 2)
        sqrt-r2 (int (Math/sqrt target-r2))
        divisors (for [d (range 1 sqrt-r2) :when (zero? (rem target-r2 d))] [d (quot target-r2 d)])]
    (apply + (flatten divisors))))