(ns tasks.02)

(def sample ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])
(def input (clojure.string/split-lines (slurp "inputs/02.txt")))

(defn counted [inp] (map frequencies inp))

(defn is-good [fq n]
  (.contains (vals fq) n))

(defn count-goods [fq n]
  (count
    (filter #(is-good % n) fq)))

(defn solve-easy [input]
  (let [cnt-inp (counted input)]
    (* (count-goods cnt-inp 2) (count-goods cnt-inp 3))))

(defn is-good-pair [pair]
  (let [mismatches (map not= (first pair) (second pair))]
    (= 1
       (count (filter identity mismatches)))))

(defn get-good-pair [input]
  (some
    #(if (is-good-pair %) %)
    (for [a input b input] [a b])))

(defn solve-hard [input]
  (let [good-pair (get-good-pair input)]
    (clojure.string/join
      (map first
           (filter #(= (first %) (second %))
                   (map vector (first good-pair) (second good-pair)))))))
