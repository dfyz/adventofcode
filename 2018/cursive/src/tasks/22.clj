(defn read-input [filename]
  (let [content (slurp filename)]
    (vec (map #(Integer/parseInt %) (drop 1 (re-find #"depth: (\d+)\ntarget: (\d+),(\d+)" content))))))

(def sample [510 10 10])
(def input (read-input "inputs\\22.txt"))

(def switch-time 7)
(def field-gap 30)

(defn erosion-level-memoized [[depth target-x target-y :as state]]
  (let [level (fn [level-mem x y]
                (let [level (partial level-mem level-mem)
                      geo-index (cond
                                  (and (zero? x) (zero? y)) 0
                                  (and (= x target-x) (= y target-y)) 0
                                  (= y 0) (* x 16807)
                                  (= x 0) (* y 48271)
                                  :else (* (level x (dec y)) (level (dec x) y)))]
                  (rem (+ geo-index depth) 20183)))
        level-mem (memoize level)]
    (partial level-mem level-mem)))

(defn create-field [state]
  (let [subgetter (erosion-level-memoized state)]
    (fn [x y] (rem (subgetter x y) 3))))

(defn solve-easy [[_ target-x target-y :as input]]
  (let [type-getter (create-field input)
        all-coords (for [x (range (inc target-x)) y (range (inc target-y))] [x y])]
    (apply + (map #(apply type-getter %) all-coords))))

(def compatibles {0 #{:gear :torch}
                  1 #{:gear :neither}
                  2 #{:torch :neither}})

(defn incompatible? [region tool]
  (nil? ((compatibles region) tool)))

(defn switch [reg tool]
  (let [do-switch (fn [option-1 option-2] (if (= tool option-2) option-1 option-2))]
    (case reg
      0 (do-switch :gear :torch)
      1 (do-switch :gear :neither)
      2 (do-switch :torch :neither))))

(defn adjacent-positions [pos target-x target-y]
  (let [deltas [[-1 0] [1 0] [0 -1] [0 1]]
        max-pos [(+ field-gap target-x) (+ field-gap target-y)]
        good-pos #(and
                    (every? (comp not neg?) %)
                    (every? true? (map <= % max-pos)))]
    (filter good-pos (mapv (partial mapv + pos) deltas))))

(defrecord Vertex [pos tool delay])

(defn adjacent-vertices [{:keys [pos tool delay] :as vertex} field target-x target-y]
  (if (not (zero? delay))
    [(assoc vertex :delay (dec delay))]
    (vec (flatten (for [pos-next (adjacent-positions pos target-x target-y)
                        :let [reg (apply field pos-next)]]
                    (if (incompatible? reg tool)
                      (mapv #(Vertex. pos % (dec switch-time)) (compatibles (apply field pos)))
                      [(Vertex. pos-next tool 0)]))))))

(defn find-shortest-path [field [target-x target-y]]
  (let [start-vertex (Vertex. [0 0] :torch 0)
        finish-vertex (Vertex. [target-x target-y] :torch 0)]
    (loop [queue (conj (clojure.lang.PersistentQueue/EMPTY) start-vertex)
           visited #{start-vertex}
           distances {start-vertex 0}
           prev {start-vertex nil}]
      (let [cur-vertex (peek queue)
            cur-dist (distances cur-vertex)]
        (if (= cur-vertex finish-vertex)
          (distances cur-vertex)
          (let [adj-vertices (filter (comp not visited) (adjacent-vertices cur-vertex field target-x target-y))]
            (recur
              (apply conj (pop queue) adj-vertices)
              (apply conj visited adj-vertices)
              (reduce #(assoc %1 %2 (inc cur-dist)) distances adj-vertices)
              (reduce #(assoc %1 %2 cur-vertex) prev adj-vertices))))))))

(defn solve-hard [input]
  (find-shortest-path (create-field input) (drop 1 input)))
