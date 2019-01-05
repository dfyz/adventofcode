(ns tasks.24)

(defrecord Group [units hp immune-to weak-to damage attack-type initiative])

(defn try-parse-extra [prefix extra]
  (if (.startsWith extra prefix)
    (set (clojure.string/split (.substring extra (count prefix)) #", "))
    nil))

(defn parse-extras [extras]
  (let [tokens (if (nil? extras) [] (clojure.string/split extras #"; "))
        extract (fn [prefix] (some #(try-parse-extra prefix %) tokens))]
    [(extract "immune to ")
     (extract "weak to ")]))

(defn parse-group [line]
  (let [re #"(\d+) units each with (\d+) hit points (?:\((.*)\) )?with an attack that does (\d+) (.*) damage at initiative (\d+)"
        [_ units hp extras damage attack-type initiative] (re-find re line)
        [immune-to-list weak-to-list] (parse-extras extras)]
    (Group. (Integer/parseInt units)
            (Integer/parseInt hp)
            immune-to-list
            weak-to-list
            (Integer/parseInt damage)
            attack-type
            (Integer/parseInt initiative))))

(defn parse-input [filename]
  (let [lines (clojure.string/split-lines (slurp filename))
        parts (split-with #(not (= % "Infection:")) lines)
        parse-lines (fn [lines] (mapv parse-group (filter #(and (seq %) (Character/isDigit (first %))) lines)))]
    (mapv parse-lines parts)))

(def sample (parse-input "inputs/24_sample.txt"))
(def input (parse-input "inputs/24.txt"))

(defn effective-power [group]
  (let [units (:units group)]
    (if (not (pos? units))
      0
      (* (:units group) (:damage group)))))

(defn damage-to [attacker defender]
  (let [attack-type (:attack-type attacker)
        multiplier (cond
                     (get (:immune-to defender) attack-type) 0
                     (get (:weak-to defender) attack-type) 2
                     :else 1)]
    (* multiplier (effective-power attacker))))

(defn best-group-index [indexes army get-priority]
  (let [with-priorities (mapv (juxt identity #(get-priority (get army %))) indexes)]
    (last (sort-by second with-priorities))))

(defn assign-defenders [attacking-army defending-army]
  (let [attack-priority (juxt effective-power :initiative)]
    (loop [attackers (set (range (count attacking-army)))
           defenders (set (range (count defending-army)))
           result []]
      (if (or (empty? attackers) (empty? defenders))
        result
        (let [[best-attacker-idx _] (best-group-index attackers attacking-army attack-priority)
              best-attacker (get attacking-army best-attacker-idx)
              defence-priority (juxt #(damage-to best-attacker %) effective-power :initiative)
              [best-defender-idx [defender-damage & _]] (best-group-index defenders defending-army defence-priority)
              attackers-next (disj attackers best-attacker-idx)]
          (if (zero? defender-damage)
            (recur attackers-next defenders result)
            (recur attackers-next (disj defenders best-defender-idx) (conj result [best-attacker-idx best-defender-idx]))))))))

(defn deal-damage [group damage]
  (let [units (:units group)
        units-next (- units (quot damage (:hp group)))]
    (assoc group :units units-next)))

(defn fight [armies]
  (let [mark-with (fn [army-index pairs] (mapv #(conj % army-index) pairs))
        pairs (set (concat
                     (mark-with 0 (apply assign-defenders armies))
                     (mark-with 1 (apply assign-defenders (reverse armies)))))
        clean-armies (fn [armies] (mapv #(filterv (comp pos? :units) %) armies))]
    (loop [pairs pairs
           armies armies]
      (if (empty? pairs)
        (clean-armies armies)
        (let [[attacker-idx defender-idx attacker-army :as best-pair] (last (sort-by
                                                                              (fn [[attacker-idx _ attacker-army]]
                                                                                (get-in armies [attacker-army attacker-idx :initiative]))
                                                                              pairs))
              attacker (get-in armies [attacker-army attacker-idx])
              defender-army (- 1 attacker-army)
              defender (get-in armies [defender-army defender-idx])
              damage (damage-to attacker defender)
              defender-next (deal-damage defender damage)]
          (recur
            (disj pairs best-pair)
            (assoc-in armies [defender-army defender-idx] defender-next)))))))

(defn get-outcome [armies]
  (loop [armies armies]
    (let [armies-next (fight armies)]
      (if (or (not-every? seq armies) (= armies armies-next))
        armies
        (recur armies-next)))))

(defn total-units [armies]
  (->> armies
       flatten
       (map :units)
       (apply +)))

(defn solve-easy [armies]
  (total-units (get-outcome armies)))

(defn boost-left-army [[army-1 army-2] boost]
  [(mapv #(assoc % :damage (+ boost (:damage %))) army-1)
   army-2])

(defn solve-hard [armies]
  (let [get-boosted-outcome #(get-outcome (boost-left-army armies %))
        smallest-boost (first (drop-while #(seq (second (get-boosted-outcome %))) (range)))]
    (total-units (get-boosted-outcome smallest-boost))))