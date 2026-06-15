;; REPL for flying the simulated rocket
(ns sim-repl)

(defn rocket-repl-eval [input] (eval input))

(defn rep []
  (try
    (print "rocket-cmd :: ") ;; prompt
    (flush)
    (let [form (read-string (read-line)) ;; READ
          result (rocket-repl-eval form)]            ;; EVAL
      (println result)                   ;; PRINT
      result)
    (catch Throwable e
      (println (str "Error: " (ex-message e))))))

(defn repl []
  (loop []
    (when-not (= :exit (rep)) (recur))))

(rep)

(repl)