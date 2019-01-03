(defn parse-desc [desc]
  (->> desc
       (re-find #"\[(\d+), (\d+), (\d+), (\d+)\]$")
       (drop 1)
       (map #(Integer/parseInt %))
       vec))

(defn parse-op [[before op after]]
  {:before (parse-desc before)
   :op     (vec (map #(Integer/parseInt %) (clojure.string/split op #" ")))
   :after  (parse-desc after)})

(defn read-easy [lines]
  (->> lines
       (filter seq)
       (partition 3)
       (filter #(clojure.string/starts-with? (first %) "Before:"))
       (map parse-op)))

(defn read-hard [content]
  (let [[_ hard-part] (clojure.string/split content #"\n\n\n\n")
        lines (clojure.string/split hard-part #"\n")
        nums (map #(clojure.string/split % #" ") lines)]
    (map
      #(vec (map (fn [x] (Integer/parseInt x)) %))
      nums)))

(defn read-input [filename]
  (let [content (slurp filename)
        easy (read-easy (clojure.string/split-lines (slurp filename)))
        hard (read-hard content)]
    (vector easy hard)))

(def sample (read-input "inputs\\16_sample.txt"))
(def input (read-input "inputs\\16.txt"))

(defn apply-op [f a-type b-type nums [_ a b c]]
  (let [fetch (fn [type val] (if (= type :imm) val (get nums val)))
        a-val (fetch a-type a)
        b-val (fetch b-type b)
        res (f a-val b-val)]
    (assoc nums c res)))

(defn fst [x y] x)
(defn greater [x y] (if (> x y) 1 0))
(defn equal [x y] (if (= x y) 1 0))

(def ops
  (reduce-kv
    (fn [m k [a b c]] (assoc m k (partial apply-op (eval a) b c))) {}
    {:addr '(+ :reg :reg)
     :addi '(+ :reg :imm)
     :mulr '(* :reg :reg)
     :muli '(* :reg :imm)
     :banr '(bit-and :reg :reg)
     :bani '(bit-and :reg :imm)
     :borr '(bit-or :reg :reg)
     :bori '(bit-or :reg :imm)
     :setr '(fst :reg :imm)
     :seti '(fst :imm :imm)
     :gtir '(greater :imm :reg)
     :gtri '(greater :reg :imm)
     :gtrr '(greater :reg :reg)
     :eqir '(equal :imm :reg)
     :eqri '(equal :reg :imm)
     :eqrr '(equal :reg :reg)}))

(defn matching-ops [data available-ops]
  (filter
    #(= (data :after) ((val %) (data :before) (data :op)))
    available-ops))

(defn solve-easy [[easy-part _]]
  (count (filter #(>= (count (matching-ops % ops)) 3) easy-part)))

(defn assign-numbers [assignments available-ops all-data]
  (if (empty? available-ops)
    assignments
    (let [[new-assignment [[new-instr _]]]
          (second (filter
                    #(= 1 (count (second %)))
                    (map #(vector % (matching-ops % available-ops)) all-data)))]
      (recur
        (assoc assignments (first (new-assignment :op)) new-instr)
        (dissoc available-ops new-instr)
        all-data))))

(defn solve-hard [[easy-part hard-part]]
  (let [assignments (assign-numbers {} ops easy-part)
        instructions (map
                       (fn [[op-num a b c]]
                         (vector (assignments op-num) a b c))
                       hard-part)]
    (reduce
      (fn [state [op a b c]] ((ops op) state (vector nil a b c)))
      [0 0 0 0]
      instructions)))