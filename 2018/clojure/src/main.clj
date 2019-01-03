(ns main)

(defn -main [& args]
  (doseq [task-name args]
    (println "Task name:" task-name)
    (let [task-symbol (symbol (str "tasks." task-name))]
      (require [task-symbol])
      (let [[input easy hard] (map #(ns-resolve task-symbol %) ['input
                                                                'solve-easy
                                                                'solve-hard])]
        (println "\tEasy answer:" (easy @input))
        (println "\tHard answer:" (hard @input))))))