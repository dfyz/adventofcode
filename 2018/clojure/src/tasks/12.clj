(ns tasks.12)

(defn to-plants [state]
  (->> state
       (map-indexed vector)
       (filter #(= \# (second %)))
       (map first)
       (apply sorted-set)))

(defn parse-input [filename]
  (let [lines (clojure.string/split-lines (slurp filename))
        initial-plants (to-plants (last (clojure.string/split (first lines) #": ")))
        patterns (drop 2 lines)
        good-lhs (->> patterns
                      (map #(drop 1 (re-find #"(.....) => (.)" %)))
                      (filter #(= "#" (second %)))
                      (map first)
                      set)]
    (vector initial-plants good-lhs)))

(def sample (parse-input "inputs\\12_sample.txt"))
(def input (parse-input "inputs\\12.txt"))

(defn plant-image [plants pos]
  (apply str
         (map #(if (contains? plants %) "#" ".") (range (- pos 2) (+ pos 3)))))

(defn step [[plants patterns]]
  (let [next-plants (->> (range (- (first plants) 5) (+ (last plants) 5))
                         (filter #(contains? patterns (plant-image plants %)))
                         (apply sorted-set))]
    (vector next-plants patterns)))

(defn solve-easy [data]
  (apply +
         (first (last
                  (take (inc 20) (iterate step data))))))

(defn normalize-plants [plants]
  (let [min-plant (apply min plants)
        normalized-plants (map #(- % min-plant) plants)]
    (vector min-plant normalized-plants)))

(defn find-stable-state [data]
  (let [pairs (partition 2 1 (iterate step data))
        [iter-num [[p1 _] _]] (first (drop-while
                                       (fn [[_ [[p1 _] [p2 _]]]] (not=
                                                                   (second (normalize-plants p1))
                                                                   (second (normalize-plants p2))))
                                       (map-indexed vector pairs)))]
    (vector (- iter-num (first p1)) (second (normalize-plants p1)))))

(defn solve-hard [data]
  (let [[iter-diff normalized] (find-stable-state data)
        first-num (- 50000000000 iter-diff)]
    (apply + (map #(+ first-num %) normalized))))
