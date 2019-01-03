(ns tasks.05)

(def sample "dabAcCaCBAcCcaDA")
(def input (clojure.string/trim-newline (slurp "inputs\\05.txt")))

(defn good-pair [ch1 ch2]
  (and
    ch1 ch2
    (not= ch1 ch2)
    (= (clojure.string/lower-case ch1) (clojure.string/lower-case ch2))))

(defn process-rec [in out]
  (if (empty? in)
    out
    (let [ch (first in) xs (rest in)]
      (if (good-pair (first out) ch)
        (recur xs (rest out))
        (recur xs (conj out ch))))))

(defn react [data] (clojure.string/join (reverse (process-rec data ()))))
(defn solve-easy [data] (count (react data)))

(defn solve-hard [data]
  (apply min
         (map
           #(solve-easy (remove #{(Character/toUpperCase %) (Character/toLowerCase %)} data))
           (set data))))
