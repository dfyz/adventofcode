(defn parse-regex [pos input]
  (loop [pos pos
         alts []
         concats []
         syms []]
    (let [ch (.charAt input pos)
          finalize-syms #(if (seq syms) (conj concats (apply str syms)) concats)]
      (case ch
        (\) \$) [(inc pos) (conj alts (finalize-syms))]
        \( (let [[pos-next regex-next] (parse-regex (inc pos) input)]
             (recur pos-next alts (conj (finalize-syms) regex-next) []))
        \| (recur (inc pos) (conj alts (finalize-syms)) [] [])
        (recur (inc pos) alts concats (conj syms ch))))))

(defn read-input [input]
  (second (parse-regex 1 input)))

(def input (read-input (clojure.string/trim-newline (slurp "inputs\\20.txt"))))
(def samples (mapv read-input ["^WNE$"
                               "^ENWWW(NEEE|SSE(EE|N))$"
                               "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
                               "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
                               "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"]))

(defrecord Graph [edges terminals])
(def initial-graph (Graph. #{} #{[0 0]}))

(defn merge-graphs [g1 g2]
  (Graph.
    (clojure.set/union (:edges g1) (:edges g2))
    (clojure.set/union (:terminals g1) (:terminals g2))))

(declare expand-graph-by-re)

(defn expand-graph-by-concats [graph concats]
  (reduce #(expand-graph-by-re %1 %2) graph concats))

(defn expand-graph-by-alts [graph alts]
  (reduce #(merge-graphs %1 (expand-graph-by-concats graph %2)) graph alts))

(defn move-by-char [pos ch]
  (mapv + pos ({\W [-1 0] \E [1 0] \N [0 -1] \S [0 1]} ch)))

(defn expand-terminal [terminal s]
  (loop [s s pos terminal edges #{}]
    (if (empty? s)
      [pos edges]
      (let [[x & xs] s
            pos-next (move-by-char pos x)]
        (recur xs pos-next (conj edges [pos-next pos] [pos pos-next]))))))

(defn expand-graph-by-literal-string [graph s]
  (loop [edges-next (:edges graph)
         terminals-next #{}
         terminals (:terminals graph)]
    (if (empty? terminals)
      (Graph. edges-next terminals-next)
      (let [[x & xs] terminals
            [term-next new-edges] (expand-terminal x s)]
        (recur
          (clojure.set/union edges-next new-edges)
          (conj terminals-next term-next)
          xs)))))

(defn expand-graph-by-re [graph re]
  (let [expander (if (string? re) expand-graph-by-literal-string expand-graph-by-alts)]
    (expander graph re)))

(defn to-graph [re]
  (let [intermediate-graph (expand-graph-by-re initial-graph re)]
    (reduce
      #(assoc %1 (first %2) (conj (get %1 (first %2) []) (second %2)))
      {}
      (:edges intermediate-graph))))

(defn get-distances [graph]
  (let [start [0 0]]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
           visited #{start}
           distances {start 0}]
      (if (empty? queue)
        distances
        (let [cur (peek queue)
              cur-dist (distances cur)
              adj (filter (comp not visited) (graph cur))]
          (recur
            (apply conj (pop queue) adj)
            (apply conj visited adj)
            (reduce #(assoc %1 %2 (inc cur-dist)) distances adj)))))))

(defn solve-both [input]
  (let [distances (->> input
                       to-graph
                       get-distances
                       vals)]
    {:easy (apply max distances)
     :hard (count (filter #(>= % 1000) distances))}))
