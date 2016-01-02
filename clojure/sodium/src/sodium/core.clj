(ns sodium.core
  (:import [nz.sodium])
  (:import [nz.sodium Operational Stream StreamSink StreamLoop Cell CellSink Transaction Node])
  (:import [java.util ArrayList Optional])
  (:gen-class))

(set! *warn-on-reflection* true)

(defmacro apply0
  [& body]
  `(proxy [nz.sodium.Lambda0] []
     (apply []
       ~@body)))

(defmacro apply1
  [args & body]
  (let [targs (into '[this] args)]
  `(reify nz.sodium.Lambda1
     (apply ~targs
       ~@body))))

(defmacro apply2
  [args & body]
  `(proxy [nz.sodium.Lambda2] []
     (apply ~args
       ~@body)))

(defmacro handler
  [args & body]
  (let [targs (into '[this] args)]
  `(reify nz.sodium.Handler
     (^void run ~targs
       ~@body))))

(defmacro transaction-handler
  [args & body]
  (let [targs (into '[this] args)]
  `(reify nz.sodium.TransactionHandler
     (run ~targs
       ~@body))))
                                                                           
(defmacro oo
  [a]
  `(Optional/of ~a))

(defmacro oe
  []
  `(Optional/empty))

