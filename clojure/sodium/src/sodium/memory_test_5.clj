(ns sodium.memory-test-5
  (:require [sodium.core :refer :all])
  (:import [nz.sodium])
  (:import [nz.sodium Operational Stream StreamSink StreamLoop Tuple2 Cell CellSink Transaction Node])
  (:import [java.util Optional])
  (:gen-class))

(defn -main
  "Memory test #5"
  [& args]
  (println "Starting!")
  (.start (Thread. (fn []
                     (loop []
                       (println (str "memory " (.totalMemory (Runtime/getRuntime))))
                       (Thread/sleep 5000)
                       (recur)))))
  (let [e-change (StreamSink.)
        out (.hold e-change 0)
        l (.listen out (handler [tt] (println tt)))]
    (loop [i 0]
      (when (< i 1000000000)
        (.send e-change i)
        (recur (inc i))))
    (.unlisten l))
  (System/exit 0))  
  
                     

