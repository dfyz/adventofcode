(def nums (map #(Integer/parseInt %) (clojure.string/split-lines (slurp "inputs\\01.txt"))))

(def easy (reduce #(+ %1 %2) nums))

(defn hard [nums]
  (loop [cur-nums (cycle nums)
         seen #{}
         cur-sum 0]
    (if
      (contains? seen cur-sum)
      cur-sum
      (recur (rest cur-nums) (conj seen cur-sum) (+ cur-sum (first cur-nums))))))

(hard '(1 -2 3 1))
(hard '(1 -1))
(hard '(3 3 4 -2 -4))
(hard '(-6 3 8 5 -6))
(hard '(7 7 -2 -7 -4))

(hard nums)