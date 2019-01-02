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
  "Converts a regex string into an itermediate representation node (IR node).
  An IR is either:
    * a literal string;
    * a two-dimensional list, the dimensons of which correspond to alternatives and concatenations, respectively.
  E.g.:
    (read-input \"^NW$\")
    => [[\"NW\"]]
    (read-input \"^NW|(N(S|N)E)$\")
    => [[\"NW\"] [[[\"N\" [[\"S\"] [\"N\"]] \"E\"]]]]"
  (second (parse-regex 1 input)))

(def input (read-input (clojure.string/trim-newline (slurp "inputs\\20.txt"))))
(def samples (mapv read-input ["^WNE$"
                               "^ENWWW(NEEE|SSE(EE|N))$"
                               "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
                               "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
                               "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"]))

;; `Graph` is our reconstruction of the maze after traversing a subset of the given IR nodes.
;;   * `edges` is a set of coordinate pairs which are known to be connected by doors;
;;   * `terminals` is a set of coordinates we can continue the reconstruction from.
(defrecord Graph [edges terminals])
(def initial-graph (Graph. #{} #{[0 0]}))
(def empty-graph (Graph. #{} #{}))

(defn merge-graphs [g1 g2]
  "Two maze reconstructions can be merged into one by simply taking a union of their members."
  (Graph.
    (clojure.set/union (:edges g1) (:edges g2))
    (clojure.set/union (:terminals g1) (:terminals g2))))

(declare expand-graph-by-re)

(defn expand-graph-by-concats [graph concats]
  "Expands `graph` by the first IR node from the given sequence of concatenated IR nodes (`concats`), obtaining a new graph,
  then expands this newly obtained graph by the second IR node, etc."
  (reduce #(expand-graph-by-re %1 %2) graph concats))

(defn expand-graph-by-alts [graph alts]
  "Expands `graph` by all possible alternatives of the IR node (`alts`), then merges the results."
  (reduce #(merge-graphs %1 (expand-graph-by-concats graph %2)) empty-graph alts))

(defn move-by-char [pos ch]
  (mapv + pos ({\W [-1 0] \E [1 0] \N [0 -1] \S [0 1]} ch)))

(defn expand-terminal [terminal path-description]
  "Walks the path corresponding to `path-description` (a literal string), starting from `terminal`.
  Returns the position we end up in along with all the edges we walk through."
  (loop [s path-description pos terminal edges #{}]
    (if (empty? s)
      [pos edges]
      (let [[x & xs] s
            pos-next (move-by-char pos x)]
        (recur xs pos-next (conj edges [pos-next pos] [pos pos-next]))))))

(defn expand-graph-by-literal-string [graph path-description]
  "Expands `graph` by `path-description` (a literal string), starting from every terminal position."
  (loop [edges-next (:edges graph)
         terminals-next #{}
         terminals (:terminals graph)]
    (if (empty? terminals)
      (Graph. edges-next terminals-next)
      (let [[x & xs] terminals
            [term-next new-edges] (expand-terminal x path-description)]
        (recur
          (clojure.set/union edges-next new-edges)
          (conj terminals-next term-next)
          xs)))))

(defn expand-graph-by-re [graph re]
  "Expands the graph by an IR node `re`, choosing the appropriate function."
  (let [expander (if (string? re) expand-graph-by-literal-string expand-graph-by-alts)]
    (expander graph re)))

(defn to-graph [re]
  "Creates a BFS'able graph from a regex string."
  (let [intermediate-graph (expand-graph-by-re initial-graph re)]
    (reduce
      #(assoc %1 (first %2) (conj (get %1 (first %2) []) (second %2)))
      {}
      (:edges intermediate-graph))))

(defn get-distances [graph]
  "Does a BFS on `graph` and returns a position->distance map, containing all reachable positions as keys."
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
