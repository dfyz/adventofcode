(ns tasks.15)

(defrecord Unit [type ap hp])

(defn parse-unit [ch]
  (Unit.
    (if (= ch \E) :elf :goblin)
    3
    200))

(defn are-enemies [unit-1 unit-2]
  (not= (:type unit-1) (:type unit-2)))

(defn is-elf [unit]
  (= :elf (:type unit)))

(defn to-field [units walls rows cols]
  [[units walls] rows cols])

(defn parse-input [filename]
  (let [lines (clojure.string/split-lines (slurp filename))
        rows (count lines)
        cols (count (first lines))]
    (loop [row 0 col 0 units (sorted-map) walls #{}]
      (if (>= row rows)
        (to-field units walls rows cols)
        (if (>= col cols)
          (recur (inc row) 0 units walls)
          (let [pos [row col]
                ch (get-in lines [row col])
                units-next (if (#{\E \G} ch)
                             (assoc units pos (parse-unit ch))
                             units)
                walls-next (if (= ch \#)
                             (conj walls pos)
                             walls)]
            (recur row (inc col) units-next walls-next)))))))

(def sample (parse-input "inputs/15_sample.txt"))
(def sample-move (parse-input "inputs/15_sample_move.txt"))
(def sample-move-2 (parse-input "inputs/15_sample_move_2.txt"))
(def sample-combat (parse-input "inputs/15_sample_combat.txt"))
(def sample-combat-2 (parse-input "inputs/15_sample_combat_2.txt"))
(def sample-combat-3 (parse-input "inputs/15_sample_combat_3.txt"))
(def sample-combat-4 (parse-input "inputs/15_sample_combat_4.txt"))
(def sample-combat-5 (parse-input "inputs/15_sample_combat_5.txt"))
(def sample-combat-6 (parse-input "inputs/15_sample_combat_6.txt"))
(def input (parse-input "inputs/15.txt"))

(defn adjacent [pos [_ rows cols]]
  (let [adj (mapv #(mapv + pos %) [[1 0] [-1 0] [0 1] [0 -1]])]
    (filter (fn [[r c]]
              (and (>= r 0) (>= c 0) (< r rows) (< c cols)))
            adj)))

(defn taken [pos [[units walls] _]]
  (or (contains? units pos) (contains? walls pos)))

(defn unseen-neighbors [pos visited field]
  (filter #(and
             (not (visited %))
             (not (taken % field)))
          (adjacent pos field)))

(defn get-distances-next [cur-dist adj distances]
  (reduce #(assoc %1 %2 (inc cur-dist)) distances adj))

(defn find-target [pos enemies [[units _] _ _ :as field]]
  (let [in-range (set (filter
                        #(or (= pos %) (not (taken % field)))
                        (mapcat
                          #(adjacent (key %) field)
                          enemies)))]
    (if (contains? in-range pos)
      pos
      (loop [queue (conj clojure.lang.PersistentQueue/EMPTY pos)
             distances {pos 0}
             visited #{pos}
             best-in-range nil]
        (if (empty? queue)
          best-in-range
          (let [cur (peek queue)
                cur-dist (get distances cur)]
            (if (and
                  (not (nil? best-in-range))
                  (> cur-dist (distances best-in-range)))
              best-in-range
              (let [adj (unseen-neighbors cur visited field)
                    distances-next (get-distances-next cur-dist adj distances)
                    visited-next (apply conj visited adj)
                    best-in-range-next (if (and
                                             (contains? in-range cur)
                                             (or
                                               (nil? best-in-range)
                                               (and
                                                 (= (distances best-in-range) cur-dist)
                                                 (neg? (compare cur best-in-range)))))
                                         cur
                                         best-in-range)]
                (recur
                  (apply conj (pop queue) adj)
                  distances-next
                  visited-next
                  best-in-range-next)))))))))

(defn move-to-target [unit-pos target-pos field]
  (if (= unit-pos target-pos)
    target-pos
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY target-pos)
           distances {target-pos 0}
           visited #{target-pos}]
      (if (empty? queue)
        (let [unit-adj (filter #(and
                                  (contains? distances %)
                                  (not (taken % field)))
                               (adjacent unit-pos field))]
          (first (sort-by #(vector (get distances %) %) unit-adj)))
        (let [cur (peek queue)
              cur-dist (get distances cur)
              adj (unseen-neighbors cur visited field)
              distances-next (get-distances-next cur-dist adj distances)
              visited-next (apply conj visited adj)]
          (recur
            (apply conj (pop queue) adj)
            distances-next
            visited-next))))))

(defn attack [unit-pos [[units _] _ _ :as field]]
  (let [unit (get units unit-pos)
        adj (adjacent unit-pos field)
        enemy-positions (filter
                          #(and
                             (contains? units %)
                             (are-enemies unit (get units %)))
                          adj)]
    (if (empty? enemy-positions)
      units
      (let [defender-pos (first (sort-by
                                  (juxt
                                    #(:hp (get units %))
                                    identity)
                                  enemy-positions))
            defender (get units defender-pos)
            hp-next (- (:hp defender) (:ap unit))]
        (if (not (pos? hp-next))
          (dissoc units defender-pos)
          (assoc
            units
            defender-pos
            (assoc defender :hp hp-next)))))))

(defn step [[[units walls] rows cols :as field]]
  (loop [start-positions (apply sorted-set (keys units))
         units units]
    (let [field (to-field units walls rows cols)]
      (if (empty? start-positions)
        [field true]
        (let [[x & xs] start-positions
              current-unit (get units x)]
          (if (nil? current-unit)
            (recur xs units)
            (let [enemies (filter #(are-enemies current-unit (val %)) units)]
              (if (empty? enemies)
                [field false]
                (let [target (find-target x enemies field)]
                  (if (nil? target)
                    (recur xs units)
                    (let [move-to (move-to-target x target field)
                          after-move (assoc
                                       (dissoc units x)
                                       move-to
                                       (get units x))
                          units-next (attack move-to (to-field after-move walls rows cols))]
                      (recur xs units-next))))))))))))

(defn get-final-units [field]
  (loop [field field
         rounds-completed 0]
    (let [[field-next full-round] (step field)
          units-next (first (first field-next))]
      (if (not full-round)
        [units-next rounds-completed]
        (recur field-next (inc rounds-completed))))))

(defn get-outcome [units rounds-completed]
  (let [hp-sum (apply + (map :hp (vals units)))]
    (* rounds-completed hp-sum)))

(defn solve-easy [field]
  (let [[final-units rounds-completed] (get-final-units field)]
    (get-outcome final-units rounds-completed)))

(defn boost-elves [[[units walls] rows cols] boosted-ap]
  (loop [boosted-units (sorted-map)
         units units]
    (if (empty? units)
      (to-field boosted-units walls rows cols)
      (let [[[pos unit] & xs] units
            old-ap (:ap unit)
            new-ap (if (is-elf unit) boosted-ap old-ap)]
        (recur
          (assoc boosted-units pos
                               (assoc unit :ap new-ap))
          xs)))))

(defn solve-hard [[[initial-units _] _ _ :as field]]
  (let [count-elves (fn [units] (count (filter #(is-elf (val %)) units)))
        initial-elf-count (count-elves initial-units)]
    (loop [boosted-ap 4]
      (let [boosted-field (boost-elves field boosted-ap)
            [final-units rounds-completed] (get-final-units boosted-field)]
        (if (= initial-elf-count (count-elves final-units))
          (get-outcome final-units rounds-completed)
          (recur (inc boosted-ap)))))))
