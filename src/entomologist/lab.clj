(ns entomologist.lab)


(def mark-agent
  (agent '() :error-handler (fn [ag ex]
                              (.printStackTrace ex))))

(defn async-mark! [x]
  (send mark-agent #(cons x %)))

(defn flush-mark-agent! []
  (send mark-agent (fn [v]
                     (println "count :" (count v))
                     '())))

(def agent-flusher-running?
  (atom false))

(defn stop-mark-agent-flusher! []
  (when @agent-flusher-running?
    (reset! agent-flusher-running? false)))

(defn start-mark-agent-flusher! []
  (when @agent-flusher-running?
    (stop-mark-agent-flusher!)
    (Thread/sleep 200))
  (reset! agent-flusher-running? true)
  (future
    (loop []
      (Thread/sleep 200)
      (flush-mark-agent!)
      (if @agent-flusher-running?
        (recur)
        (println "Stopped flusher")))))

; (restart-agent mark-agent '())
(start-mark-agent-flusher!)
(future
  (Thread/sleep 2000)
  (stop-mark-agent-flusher!))
(doseq [x (range 10000)]
  (async-mark! :x))


