(ns tasks.07)

(def sample ["Step C must be finished before step A can begin."
             "Step C must be finished before step F can begin."
             "Step A must be finished before step B can begin."
             "Step A must be finished before step D can begin."
             "Step B must be finished before step E can begin."
             "Step D must be finished before step E can begin."
             "Step F must be finished before step E can begin."])

(def input (clojure.string/split-lines (slurp "inputs\\07.txt")))

(defn parse-input [lines]
  (map
    #(drop 1 (re-find #"Step (.) must be finished before step (.) can begin." %))
    lines))

(defn all-nodes [edges]
  (apply sorted-set (flatten edges)))

(defn extract-next [nodes edges]
  (let [have-incoming (set (map second edges))]
    (first
      (filter
        #(not (contains? have-incoming %))
        nodes))))

(defn remove-node [edges n]
  (filter
    #(not= n (first %))
    edges))

(defn one-step [[answer nodes edges]]
  (let [node-next (extract-next nodes edges)]
    [(str answer node-next) (disj nodes node-next) (remove-node edges node-next)]))

(defn solve-easy [data]
  (let [edges (parse-input data)]
    (first
      (first
        (drop-while
          #(seq (second %))
          (iterate
            one-step
            ["" (all-nodes edges) edges]))))))

(defn edges->graph [edges]
  {:edges edges :nodes (all-nodes edges)})

(defn node->work [node base-time]
  (let [to-char (fn [s] (int (.charAt s 0)))
        work-units (+ 1 base-time (- (to-char node) (to-char "A")))]
    (apply str (repeat work-units node))))

(defn update-work [{:keys [nodes edges] :as graph} work base-time]
  (case (count work)
    0 (let [node-next (extract-next nodes edges)
            work-next (if node-next (node->work node-next base-time) nil)
            nodes-next (if node-next (disj nodes node-next) nodes)]
        [(assoc graph :nodes nodes-next) work-next])
    1 (let [graph-next
            (assoc graph :edges
                         (remove-node edges work))]
        (update-work graph-next nil base-time))
    [graph (subs work 1)]))

(defn update-workers [graph workers base-time]
  (loop [graph graph
         workers workers
         idx 0]
    (if (>= idx (count workers))
      [graph workers]
      (let [work (get workers idx)
            [graph-next work-next] (update-work graph work base-time)]
        (recur graph-next (assoc workers idx work-next) (inc idx))))))

(defn simulate-work [graph base-time worker-count]
  (loop [graph graph
         workers (vec (repeat worker-count nil))
         step -1]
    (if (and (empty? (graph :nodes)) (every? nil? workers))
      step
      (let [[graph-next workers-next] (update-workers graph workers base-time)]
        (recur graph-next workers-next (inc step))))))

(defn solve-hard [data]
  (let [graph (edges->graph (parse-input data))]
    (simulate-work graph 60 5)))
