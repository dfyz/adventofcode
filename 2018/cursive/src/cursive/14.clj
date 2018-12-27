(defn combine [nums x y]
  (let [sum (+ x y)
        little (rem sum 10)
        big (quot sum 10)]
    (conj
      (if (zero? big) nums (conj nums big))
      little)))

(defn step [[nums idx1 idx2]]
  (let [score1 (get nums idx1)
        score2 (get nums idx2)
        nums-next (combine nums score1 score2)
        get-idx-next (fn [idx score] (rem (+ 1 idx score) (count nums-next)))]
    [nums-next (get-idx-next idx1 score1) (get-idx-next idx2 score2)]))

(def initial-state [[3 7] 0 1])

(defn until-10-after [n]
  (let [count-needed (+ n 10)]
    (first (drop-while #(< (count (first %)) count-needed) (iterate step initial-state)))))

(defn nums->str [nums]
  (apply str (map str nums)))

(defn str->nums [s]
  (vec (map #(Integer/parseInt (str %)) s)))

(defn solve-easy [input]
  (let [n (Integer/parseInt input)
        state-10-after (until-10-after n)
        last-10 (take 10 (drop n (first state-10-after)))]
    (nums->str last-10)))

(defn until-suffix [suffix]
  (let [not-good (fn [nums]
                   (or
                     (< (count nums) (count suffix))
                     (not= suffix (subvec nums (- (count nums) (count suffix))))))
        drop-last-digit (fn [nums] (subvec nums 0 (dec (count nums))))
        with-suffix (first (first (drop-while
                                    #(and (not-good (first %)) (not-good (drop-last-digit (first %))))
                                    (iterate step initial-state))))
        with-proper-suffix (first (drop-while
                                    not-good
                                    (iterate drop-last-digit with-suffix)))]
    (count (drop (count suffix) with-proper-suffix))))

(defn solve-hard [input]
  (until-suffix (str->nums input)))
