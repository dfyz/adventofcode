(ns tasks.06)

(def raw-sample ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"])
(def raw-input (clojure.string/split-lines (slurp "inputs/06.txt")))

(defn parse-input [lines]
  (->> lines
       (map #(clojure.string/split % #", "))
       (map
         #(map (fn [x] (Integer/parseInt x)) %))))

(def sample (parse-input raw-sample))
(def input (parse-input raw-input))

(defn distance [p1 p2]
  (let [diff (fn [c1 c2] (Math/abs (- c1 c2)))]
    (+
      (diff (first p1) (first p2))
      (diff (second p1) (second p2)))))

(defn find-nearest [p points]
  (let [[[p1 d1] [_ d2]] (sort-by
                           second
                           (map
                             #(vector % (distance p %))
                             points))]
    (if (= d1 d2)
      nil
      p1)))

(defn bbox [points]
  (let [xs (map first points) ys (map second points)]
    [(apply min xs)
     (apply max xs)
     (apply min ys)
     (apply max ys)]))

(defn point-range [points]
  (let [[x-min x-max y-min y-max] (bbox points)]
    (for
      [x (range x-min (+ x-max 1))
       y (range y-min (+ y-max 1))]
      [x y])))

(defn update-areas [areas cand points]
  (let [nearest (find-nearest cand points)]
    (assoc areas nearest (inc (get areas nearest 0)))))

(defn count-areas [points]
  (let [cands (point-range points)]
    (reduce #(update-areas %1 %2 points) {} cands)))

(defn outer-ring [points]
  (let [[x-min x-max y-min y-max] (bbox points)]
    (let [outer-x-min (dec x-min)
          outer-x-max (inc x-max)
          outer-y-min (dec y-min)
          outer-y-max (inc y-max)]
      (concat
        (for [x (range outer-x-min (inc outer-x-max))
              y [outer-y-min outer-y-max]]
          [x y])
        (for [y (range outer-y-min (inc outer-y-max))
              x [outer-x-min outer-x-max]]
          [x y])))))

(defn infinities [points]
  (let [ring (outer-ring points)]
    (set (map #(find-nearest % points) ring))))

(defn solve-easy [points]
  (apply max
         (vals
           (apply dissoc (count-areas points) (infinities points)))))

(defn is-safe [p points max-dist]
  (<
    (apply + (map #(distance p %) points))
    max-dist))

(defn count-safe [points max-dist]
  (let [cands (point-range points)]
    (count
      (filter #(is-safe % points max-dist) cands))))

(defn solve-hard [points]
  (count-safe points 10000))
