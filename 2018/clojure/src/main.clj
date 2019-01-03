(ns main)

(defn print-answers [task-name easy-answer hard-answer]
  (println "\tEasy answer:" (if (= task-name "10") (str "\n" easy-answer) easy-answer))
  (println "\tHard answer:" hard-answer))

(defn -main [& args]
  (doseq [task-name args]
    (println "Task name:" task-name)
    (let [task-symbol (symbol (str "tasks." task-name))]
      (require [task-symbol])
      (let [input (ns-resolve task-symbol 'input)]
        (case task-name
          "20" (apply print-answers ((ns-resolve task-symbol 'solve-both) @input))
          "25" (print-answers task-name ((ns-resolve task-symbol 'solve-easy) @input) "Merry Christmas!")
          (let [[easy hard] (map #(ns-resolve task-symbol %) ['solve-easy 'solve-hard])]
            (print-answers task-name (easy @input) (hard @input))))))))
