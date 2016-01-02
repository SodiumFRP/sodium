(ns sodium.memory-test-3
  (:require [sodium.core :refer :all])
  (:import [nz.sodium])
  (:import [nz.sodium Operational Stream StreamSink StreamLoop Tuple2 Cell CellSink Transaction Node])
  (:import [java.util Optional])
  (:gen-class))



(defn -main
  "Memory test #3"
  [& args]
  (println "Starting!")
  (.start (Thread. (fn []
                     (loop []
                       (println (str "memory " (.totalMemory (Runtime/getRuntime))))
                       (Thread/sleep 5000)
                       (recur)))))
  (let [et (StreamSink.)
        t (.hold et 0)
        e-change (StreamSink.)
        oout (.hold (.map e-change (apply1 [x] ())) t)
        out (Cell/switchC oout)
        l (.listen out (handler [tt] ()))]
    (loop [i 0]
      (when (< i 1000000000)
        (.send et i)
        (recur (inc i))))
    (.unlisten l))
  (System/exit 0))
  
                     

