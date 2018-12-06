(def sample
  ["[1518-11-01 00:00] Guard #10 begins shift"
   "[1518-11-01 00:05] falls asleep"
   "[1518-11-01 00:25] wakes up"
   "[1518-11-01 00:30] falls asleep"
   "[1518-11-01 00:55] wakes up"
   "[1518-11-01 23:58] Guard #99 begins shift"
   "[1518-11-02 00:40] falls asleep"
   "[1518-11-02 00:50] wakes up"
   "[1518-11-03 00:05] Guard #10 begins shift"
   "[1518-11-03 00:24] falls asleep"
   "[1518-11-03 00:29] wakes up"
   "[1518-11-04 00:02] Guard #99 begins shift"
   "[1518-11-04 00:36] falls asleep"
   "[1518-11-04 00:46] wakes up"
   "[1518-11-05 00:03] Guard #99 begins shift"
   "[1518-11-05 00:45] falls asleep"
   "[1518-11-05 00:55] wakes up"])

(def input (clojure.string/split-lines (slurp "04\\input.txt")))

(defn parse-event [event]
  (case event
    "falls asleep" :asleep
    "wakes up"     :awake
    (Integer/parseInt (second (re-find #"(\d+)" event)))))

(defn parse-line [line]
  (let [groups
        (re-find #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)$" line)]
    (conj
     (map #(Integer/parseInt %) (drop-last (rest groups)))
     (parse-event (last groups)))))

(defn parse-input [input]
  (->> input
       (map parse-line)
       (sort-by #(vec (drop 1 %)))))

(def parsed-sample (parse-input sample))
(def parsed-input (parse-input input))

(defn update-with-minute [minutes m]
  (assoc minutes m (inc (get minutes m 0))))

(defn update-state [state prev-line cur-line cur-id]
  (let [cur-type (first cur-line)]
    (do
      (if (= cur-type :awake)
        (do
          (assert (= (first prev-line) :asleep))
          (reduce
            #(assoc %1 cur-id (update-with-minute (get %1 cur-id) %2))
            state
            (range (last prev-line) (last cur-line))))
        state))))

(defn find-asleep-minutes [data]
  (loop [cur-id nil
        prev-line nil
        lst data
        state {}]
    (let [cur-line (first lst)
          new-state (update-state state prev-line cur-line cur-id)]
      (if (empty? (rest lst))
        new-state
        (let [new-id (if (integer? (first cur-line)) (first cur-line) cur-id)]
          (recur new-id cur-line (rest lst) new-state))))))

(defn easy-answer [data]
  (let [best-guard
          (apply max-key #(apply + (vals (val %))) (find-asleep-minutes data))]
    (let [best-minute
            (first (apply max-key #(val %) (val best-guard)))]
      (* (first best-guard) best-minute))))

(easy-answer parsed-input)

(defn hard-answer [data]
  (letfn [(find-max-minute [minutes]
            (apply max-key val (val minutes)))]
    (let [best-guard
          (apply max-key #(val (find-max-minute %)) (find-asleep-minutes data))]
      (*
        (first best-guard)
        (first (find-max-minute best-guard))))))

(hard-answer parsed-input)