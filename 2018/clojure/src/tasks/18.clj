(ns tasks.18)

(def input (vec (map vec (clojure.string/split-lines (slurp "inputs/18.txt")))))

(defn g [data row col]
  (get (get data row) col))

(defn get-area [data row col]
  (let [adj (for [r (range (dec row) (+ 2 row))
                  c (range (dec col) (+ 2 col))
                  :let [val (g data r c)]
                  :when (and val (or (not= r row) (not= c col)))]
              val)]
    (vector (g data row col) (frequencies adj))))

(defn transform [sym adj]
  (let [check (fn [s cnt] (>= (adj s 0) cnt))]
    (case sym
      \. (if (check \| 3) \| \.)
      \| (if (check \# 3) \# \|)
      \# (if (and (check \# 1) (check \| 1)) \# \.))))

(defn step [data]
  (vec (for [r (range (count data))]
         (vec (for [c (range (count data))]
                (apply transform (get-area data r c)))))))

(defn field-to-counts [field]
  (reduce #(merge-with + %1 %2) (map frequencies field)))

(defn profit [total-freqs]
  (* (total-freqs \|) (total-freqs \#)))

(defn solve-easy [data]
  (let [final (last (take (inc 10) (iterate step data)))]
    (profit (field-to-counts final))))

(defn get-period [data]
  (let [prefix (reverse (take 1000 (map-indexed
                                     #(vector %1 (profit (field-to-counts %2)))
                                     (iterate step data))))]
    (loop [used #{} period [] xs prefix]
      (let [[_ val :as item] (first xs)]
        (if (contains? used val)
          (reverse period)
          (recur (conj used val) (conj period item) (rest xs)))))))

(defn solve-hard [data]
  (let [period (get-period data)
        period-len (count period)]
    (second (first (filter #(= (rem (first %) period-len) (rem 1000000000 period-len)) period)))))
