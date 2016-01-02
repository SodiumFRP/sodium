(ns sodium.memory-test-1
  (:require [sodium.core :refer :all])
  (:import [nz.sodium])
  (:import [nz.sodium Operational Stream StreamSink StreamLoop Tuple2 Cell CellSink Transaction Node])
  (:import [java.util Optional])
  (:gen-class))



(defn -main
  "Memory test #1"
  [& args]
  (println "Starting!")
  (.start (Thread. (fn []
                     (loop []
                       (println (str "memory " (.totalMemory (Runtime/getRuntime))))
                       (Thread/sleep 5000)
                       (recur)))))
  (let [et (StreamSink.)
        t (.hold et 0)
        etens (.map et (apply1 [x] (int (/ x 10))))
        change-tens (Stream/filterOptional (.snapshot et t (apply2 [neu old]
                                                                  (if (= neu old)
                                                                    (oe)
                                                                    (oo neu)))))
        oout (.hold (.map change-tens (apply1 [tens]
                                             (.map t (apply1 [tt]
                                                             (Tuple2. tens tt)))))
                    (.map t (apply1 [tt]
                                    (Tuple2. 0 tt))))
        out (Cell/switchC oout)
        l (.listen out (handler [tu] (comment println (str (.a tu) "," (.b tu)))))]

    (loop [i 0]
      (when (< i 1000000000)
        (.send et i)
        (recur (inc i))))
    (.unlisten l))
  (System/exit 0))
  
                     

