(ns tasks.08)

(defn parse-tree [nums]
  (let [[child-count metadata-count & other] nums]
    (loop [children []
           children-left child-count
           nums-left other]
      (if (= 0 children-left)
        (let [[metadata parsed-left] (split-at metadata-count nums-left)
              parsed {:metadata metadata
                      :children children}]
          [parsed parsed-left])
        (let [[child-tree nums-left-next] (parse-tree nums-left)]
          (recur
            (conj children child-tree)
            (dec children-left)
            nums-left-next))))))

(defn input->tree [input] (first (parse-tree input)))

(def sample (input->tree [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2]))
(def input (input->tree (map #(Integer/parseInt %) (clojure.string/split (clojure.string/trim-newline (slurp "inputs/08.txt")) #" "))))

(defn solve-easy [tree]
  (let [meta-sum (apply + (tree :metadata))]
    (+ meta-sum (apply + (map solve-easy (tree :children))))))

(defn solve-hard [tree]
  (let [{children :children metadata :metadata} tree]
    (if (empty? children)
      (apply + metadata)
      (let [child-values (vec (map solve-hard children))]
        (apply + (map #(get child-values (dec %) 0) metadata))))))
