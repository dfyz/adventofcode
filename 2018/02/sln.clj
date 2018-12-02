(def sample ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])
(def inp (clojure.string/split-lines (slurp "02\\input.txt")))

(defn counted [inp] (map frequencies inp))

(defn is-good [fq n]
    (.contains (vals fq) n))

(defn count-goods [fq n]
    (count
        (filter #(is-good % n) fq)))

(def easy
    (let [cnt-inp (counted inp)]
        (* (count-goods cnt-inp 2) (count-goods cnt-inp 3))))

(defn is-good-pair [pair]
    (let [mismatches (map not= (first pair) (second pair))]
        (= 1
            (count (filter identity mismatches)))))

(def good-pair
    (some
        #(if (is-good-pair %) %)
        (for [a inp b inp] [a b])))

(def hard
    (clojure.string/join
        (map first
            (filter #(= (first %) (second %))
                (map vector (first good-pair) (second good-pair))))))

[easy hard]