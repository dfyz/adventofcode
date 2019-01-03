(ns tasks.01)

(def input (map #(Integer/parseInt %) (clojure.string/split-lines (slurp "inputs\\01.txt"))))

(defn solve-easy [nums] (reduce #(+ %1 %2) nums))

(defn solve-hard [nums]
  (loop [cur-nums (cycle nums)
         seen #{}
         cur-sum 0]
    (if
      (contains? seen cur-sum)
      cur-sum
      (recur (rest cur-nums) (conj seen cur-sum) (+ cur-sum (first cur-nums))))))
