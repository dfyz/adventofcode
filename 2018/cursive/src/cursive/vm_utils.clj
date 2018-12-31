(ns cursive.vm-utils)

(defn parse-line [line]
  (let [[op a b c] (clojure.string/split line #" ")]
    [op (Integer/parseInt a) (Integer/parseInt b) (Integer/parseInt c)]))

(defn parse-input [filename]
  (let [[x & xs] (clojure.string/split-lines (slurp filename))
        ip-reg (Integer/parseInt (last (clojure.string/split x #" ")))
        parsed-lines (vec (map parse-line xs))]
    [parsed-lines ip-reg]))

(defn disassemble [[code ip-reg]]
  (let [reg-name (fn [idx] (if (= idx ip-reg) "IP" (str "R" idx)))
        disassemble-line (fn [op a b c] (let [ra (reg-name a)
                                              rb (reg-name b)
                                              rc (reg-name c)
                                              ternary " ? 1 : 0"
                                              suffix (case op
                                                       "seti" [a]
                                                       "setr" [ra]
                                                       "addi" [ra " + " b]
                                                       "addr" [ra " + " rb]
                                                       "muli" [ra " * " b]
                                                       "mulr" [ra " * " rb]
                                                       "bani" [ra " & " b]
                                                       "bori" [ra " | " b]
                                                       "eqrr" [ra " == " rb ternary]
                                                       "eqri" [ra " == " b ternary]
                                                       "gtrr" [ra " > " rb ternary]
                                                       "gtir" [a " > " rb ternary])]
                                          (apply str (vec (concat [rc " = "] suffix)))))]
    (map-indexed #(str %1 "\t" (apply disassemble-line %2)) code)))

(defn apply-op [f a-type b-type a b c nums]
  (let [fetch (fn [type val] (if (= type :imm) val (get nums val)))
        a-val (fetch a-type a)
        b-val (fetch b-type b)
        res (f a-val b-val)]
    (assoc nums c res)))

(defmacro gen-ops
  [& args]
  (loop [res (list) args args]
    (if (empty? args)
      `(def ops (hash-map ~@res))
      (let [[op-name op a-type b-type & xs] args
            real-op (case op
                      & bit-and
                      | bit-or
                      1 (fn [x _] x)
                      > (fn [x y] (if (> x y) 1 0))
                      = (fn [x y] (if (= x y) 1 0))
                      op)]
        (recur
          (concat res `((name ~op-name) (partial apply-op ~real-op ~a-type ~b-type)))
          xs)))))

(gen-ops
  :addr + :reg :reg
  :addi + :reg :imm
  :mulr * :reg :reg
  :muli * :reg :imm
  :banr & :reg :reg
  :bani & :reg :imm
  :borr | :reg :reg
  :bori | :reg :imm
  :setr 1 :reg :imm
  :seti 1 :imm :imm
  :gtir > :imm :reg
  :gtri > :reg :imm
  :gtrr > :reg :reg
  :eqir = :imm :reg
  :eqri = :reg :imm
  :eqrr = :reg :reg)

(defn step [[code ip-reg] ip regs]
  (let [[op-name a b c] (get code ip)
        regs ((ops op-name) a b c (assoc regs ip-reg ip))
        ip (inc (get regs ip-reg))]
    [ip regs]))
