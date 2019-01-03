(ns tasks.23)

(defn parse-bot [line]
  (let [[_ x y z r] (re-find #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" line)]
    [(Integer/parseInt x)
     (Integer/parseInt y)
     (Integer/parseInt z)
     (Integer/parseInt r)]))

(defn parse-input [filename]
  (let [lines (clojure.string/split-lines (slurp filename))]
    (map parse-bot lines)))

(def sample (parse-input "inputs\\23_sample.txt"))
(def sample-1 (parse-input "inputs\\23_sample1.txt"))
(def input (parse-input "inputs\\23.txt"))

(defn d [c1 c2] (Math/abs (- c1 c2)))

(defn dist [[x1 y1 z1 _] [x2 y2 z2 _]]
  (+ (d x1 x2) (d y1 y2) (d z1 z2)))

(defn best-bot [bots]
  (apply max-key #(last %) bots))

(defn solve-easy [bots]
  (let [bb (best-bot bots)]
    (count (filter #(<= (dist % bb) (last bb)) bots))))

(defn solve-hard [bots]
  (let [ctx (com.microsoft.z3.Context. {"model" "true"})
        opt (.mkOptimize ctx)
        mk-var #(.mkIntConst ctx %)
        mk-const #(.mkInt ctx %)
        mk-abs #(.mkITE ctx (.mkGe ctx % (mk-const 0)) % (.mkUnaryMinus ctx %))
        into-args #(into-array com.microsoft.z3.ArithExpr %)
        mk-coord-diff (fn [var coord] (mk-abs (.mkSub ctx (into-args [var (mk-const coord)]))))
        [x-exp y-exp z-exp :as exps] (map mk-var ["x" "y" "z"])]
    (do
      (doseq [[x y z r] bots]
        (let [coord-sum (.mkAdd ctx (into-args [(mk-coord-diff x-exp x)
                                                (mk-coord-diff y-exp y)
                                                (mk-coord-diff z-exp z)]))
              bot-expr (.mkLe ctx coord-sum (mk-const r))]
          (.AssertSoft opt bot-expr 1 "bots")))
      (.Check opt (into-array com.microsoft.z3.Expr []))
      (let [model (.getModel opt)
            get-int #(.getInt (cast com.microsoft.z3.IntNum (.eval model % false)))]
        (apply + (mapv get-int exps))))))
