(def field-size 300)

(defn power-level [x y sn]
  (let [rack-id (+ x 10)]
    (->>
      rack-id
      (* y)
      (+ sn)
      (* rack-id)
      (#(quot (rem % 1000) 100))
      (#(- % 5)))))

(defn square-power-level [x y sn]
  (let [squares (for [xx (range x (+ x 3)) yy (range y (+ y 3))] [xx yy])]
    (apply + (map #(power-level (first %) (second %) sn) squares))))

(defn solve-easy [sn]
  (let [cands (for [x (range 1 (dec field-size)) y (range 1 (dec field-size))] [x y])]
    (apply max-key #(square-power-level (first %) (second %) sn) cands)))

(defn cum-sum [x y sn]
  (if (or (<= x 0) (<= y 0))
    0
    (let [add1 (cum-sum (dec x) y sn)
          add2 (cum-sum x (dec y) sn)
          sub (cum-sum (dec x) (dec y) sn)]
      (-
        (+ add1 add2 (power-level x y sn))
        sub))))

(def cum-sum (memoize cum-sum))

(defn square-sum [x y sz sn]
  (let [first-out-x (dec x)
        first-out-y (dec y)
        last-in-x (dec (+ x sz))
        last-in-y (dec (+ y sz))
        a (cum-sum first-out-x first-out-y sn)
        b (cum-sum first-out-x last-in-y sn)
        c (cum-sum last-in-x first-out-y sn)
        d (cum-sum last-in-x last-in-y sn)]
    (+ a (- b) (- c) d)))

(defn solve-hard [sn]
  (let [cands (for [x (range 1 (inc field-size))
                    y (range 1 (inc field-size))
                    size (range 1 (min
                                    (- (inc field-size) x)
                                    (- (inc field-size) y)))]
                [x y size])]
    (apply
      max-key
      (fn [[x y sz]] (square-sum x y sz sn))
      cands)))
