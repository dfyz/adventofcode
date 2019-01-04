(ns tasks.13)

(defn read-input [filename]
  (let [lines (vec (map vec (clojure.string/split-lines (slurp filename))))]
    lines))

(defn remove-cart [field row col ch]
  (let [ch-next (case ch
                  (\v \^) \|
                  \-)]
    (assoc field row (assoc (get field row) col ch-next))))

(defn collect-carts [field]
  (let [rows (count field)
        cols (count (first field))]
    (loop [row 0
           col 0
           carts (sorted-map)
           field field]
      (if (and (>= row rows) (>= col cols))
        [carts field]
        (if (>= col cols)
          (recur (inc row) 0 carts field)
          (let [ch (get (get field row) col)
                [carts-next field-next] (case ch
                                          (\< \> \^ \v) [(conj carts {[row col] {:state  ch
                                                                                 :memory :left}})
                                                         (remove-cart field row col ch)]
                                          [carts field])]
            (recur row (inc col) carts-next field-next)))))))

(defn parse-input [filename]
  (collect-carts (read-input filename)))

(def sample (parse-input "inputs/13_sample.txt"))
(def sample-hard (parse-input "inputs/13_sample_hard.txt"))
(def input (parse-input "inputs/13.txt"))

(def memory-cycle [:left :straight :right])
(def state-cycle [\> \v \< \^])

(defn rotate [cycle val direction]
  (let [pos (.indexOf cycle val)
        idx-next (rem (+ pos direction (count cycle)) (count cycle))]
    (get cycle idx-next)))

(defn turn-left [state] (rotate state-cycle state -1))
(defn turn-right [state] (rotate state-cycle state 1))

(defn handle-intersection [{:keys [state memory]}]
  {:state  (case memory
             :left (turn-left state)
             :straight state
             :right (turn-right state))
   :memory (rotate memory-cycle memory 1)})

(defn handle-corner [{:keys [state] :as cart} ch]
  (let [state-next (case [state ch]
                     ([\> \/] [\v \\] [\< \/] [\^ \\]) (turn-left state)
                     ([\> \\] [\v \/] [\< \\] [\^ \/]) (turn-right state))]
    (assoc cart :state state-next)))

(defn move-cart [[[row col] cart] field]
  (let [state (cart :state)
        row-next (+ row (get {\^ -1 \v 1} state 0))
        col-next (+ col (get {\< -1 \> 1} state 0))
        sym-next (get (get field row-next) col-next)
        cart-next (case sym-next
                    \+ (handle-intersection cart)
                    (\\ \/) (handle-corner cart sym-next)
                    (\- \|) cart)]
    [[row-next col-next] cart-next]))

(defn tick [[carts field] kind]
  (let [carts-next (loop [remaining-carts carts
                          after remaining-carts]
                     (if (empty? remaining-carts)
                       after
                       (let [current-cart (first remaining-carts)
                             [current-cart-pos _] current-cart
                             [key val] (move-cart current-cart field)
                             without-current-cart (into (sorted-map) (rest remaining-carts))]
                         (if (contains? after key)
                           (if (= kind :easy)
                             (sorted-map key nil)
                             (recur
                               (dissoc without-current-cart key)
                               (dissoc after key current-cart-pos)))
                           (recur
                             without-current-cart
                             (dissoc (assoc after key val) current-cart-pos))))))]
    [carts-next field]))

(defn solve [state kind]
  (let [[crash-row crash-col] (last (take 5 (iterate first (drop-while
                                                             #(> (count (first %)) 1)
                                                             (iterate #(tick % kind) state)))))]
    (str crash-col "," crash-row)))

(defn solve-easy [state] (solve state :easy))
(defn solve-hard [state] (solve state :hard))
