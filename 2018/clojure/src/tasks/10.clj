(ns tasks.10)

(defn parse-input [filename]
  (let [content (clojure.string/split-lines (slurp filename))]
    (->> content
         (map #(drop 1 (re-find #".*?(-?\d+).*?(-?\d+).*?(-?\d+).*?(-?\d+)" %)))
         (map (fn [x] (map #(Integer/parseInt %) x)))
         (map #(vector (take 2 %) (drop 2 %))))))

(def sample (parse-input "inputs/10_sample.txt"))
(def input (parse-input "inputs/10.txt"))

(defn step [data]
  (map #(vector (apply mapv + %) (second %)) data))

(defn bounds [data]
  (let [xs (map first data)
        ys (map second data)]
    (vector (apply min xs) (apply max xs) (apply min ys) (apply max ys))))

(defn area [data]
  (let [[x-min x-max y-min y-max] (bounds data)]
    (* (- x-max x-min) (- y-max y-min))))

(defn differences [data]
  (map #(vector (->> % first first) (- (->> % second second) (->> % first second)))
       (partition 2 1
                  (map
                    #(vector % (area (map first %)))
                    (iterate step data)))))

(defn find-candidate [data]
  (map first
       (first
         (first
           (drop-while #(< (second %) 0) (differences data))))))


(defn solve-easy [data]
  (let [cand (set (find-candidate data))
        [x-min x-max y-min y-max] (bounds cand)]
    (clojure.string/join
      "\n"
      (for [y (range y-min (inc y-max))]
        (clojure.string/join
          (for [x (range x-min (inc x-max))]
            (if
              (contains? cand [x y])
              "#"
              ".")))))))

(defn solve-hard [data]
  (count (take-while #(< (second %) 0) (differences data))))
