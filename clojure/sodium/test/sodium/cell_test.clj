(ns sodium.cell-test
  (:require [clojure.test :refer :all]
            [sodium.core :refer :all])
  (:import [nz.sodium Operational Stream StreamSink CellSink Cell CellLoop Listener Transaction Unit Node])
  (:import [java.util Optional]))

(set! *warn-on-reflection* true)

(deftest test-hold
  (testing "hold"
    (let [e (StreamSink.)
          b (.hold e 0)
          out (atom[])
          l (.listen (Operational/updates b) (handler [x] (swap! out conj x)))]
      (.send e 2)
      (.send e 9)
      (.unlisten l)
      (is (= @out [2 9])))))

(deftest test-snapshot
  (testing "snapshot"
    (let [b (CellSink. 0)
          trigger (StreamSink.)      
          out (atom[])
          l (.listen (.snapshot trigger b (apply2 [a b] (str a " " b))) (handler [x] (swap! out conj x)))]
      (.send trigger 100)
      (.send b 2)
      (.send trigger 200)
      (.send b 9)
      (.send b 1)
      (.send trigger 300)
      (.unlisten l)
      (is (= @out ["100 0" "200 2" "300 1"])))))

(deftest test-values
  (testing "values"
    (let [b (CellSink. 9)
          out (atom[])
          l (.listen b (handler [x] (swap! out conj x)))]
      (.send b 2)
      (.send b 7)
      (.unlisten l)
      (is (= @out [9 2 7])))))

(deftest constant-behavior-test
  (testing "constant behavior"
    (let [b (Cell. 12)
          out (atom[])
          l (.listen b (handler [x] (swap! out conj x)))]
      (.unlisten l)
      (is (= @out [12])))))

(deftest test-value-then-map
  (testing "value then map"
    (let [b (CellSink. 9)
          out (atom[])
          l (Transaction/run (apply0 (.listen (.map (Operational/value b) (apply1 [x] (+ x 100))) (handler [x] (swap! out conj x)))))]
      (.send b 2)
      (.send b 7)
      (.unlisten ^Listener l)
      (is (= @out [109, 102, 107])))))

(deftest test-values-then-merge
  (testing "value then merge"
    (let [bi (CellSink. 9)
          bj (CellSink. 2)
          out (atom[])
          l (Transaction/run (apply0 (.listen (.merge (Operational/value bi) (Operational/value bj) (apply2 [x y] (+ x y))) (handler [x] (swap! out conj x)))))]
      (.send bi 1)
      (.send bj 4)
      (.unlisten ^Listener l)
      (is (= @out [11, 1, 4])))))

(deftest test-values-then-filter
  (testing "values then filter"
    (let [b (CellSink. 9)
          out (atom[])
          l (Transaction/run (apply0 (.listen (.filter (Operational/value b) (apply1 [a] true)) (handler [x] (swap! out conj x)))))]
      (.send b 2)
      (.send b 7)
      (.unlisten ^Listener l)
      (is (= @out [9, 2, 7])))))

(deftest test-values-then-once
  (testing "values then once"
    (let [b (CellSink. 9)
          out (atom[])
          l (Transaction/run (apply0 (.listen (.once (Operational/value b)) (handler [x] (swap! out conj x)))))]
      (.send b 2)
      (.send b 7)
      (.unlisten ^Listener l)
      (is (= @out [9])))))

(deftest test-values-late-listen
  (testing "values late listen"
    (let [b (CellSink. 9)
          out (atom[])
          value (Operational/value b)
          _ (.send b 8)
          l (.listen value (handler [x] (swap! out conj x)))]
      (.send b 2)
      (.unlisten l)
      (is (= @out [2])))))

(deftest test-map-b
  (testing "map b"
    (let [b (CellSink. 6)
          out (atom[])
          l (.listen (.map b (apply1 [x] (str x))) (handler [x] (swap! out conj x)))]
      (.send b 8)
      (.unlisten l)
      (is (= @out ["6" "8"])))))

(deftest test-map-b-late-listen
  (testing "map b late listen"
    (let [b (CellSink. 6)
          out (atom[])
          bm (.map b (apply1 [x] (str x)))
          _ (.send b 2)
          l (.listen bm (handler [x] (swap! out conj x)))]
      (.send b 8)
      (.unlisten l)
      (is (= @out ["2" "8"])))))

(comment
(deftest test-transaction
  (testing "[todo] transaction"
    (let [calledBack (make-array Boolean/TYPE 1)]
      (Transaction/run (apply0
                        (handler [trans] (do (println "in first first one") (.prioritized trans Node/NULL (apply1 [trans2] (do (println "in first one")
                                                                                            (aset-boolean calledBack 0 true))))))))
    (is (= (aget calledBack 0) false))))))


(deftest test-apply
  (testing "apply"
    (let [bf (CellSink. (apply1 [b] (str "1 " b)))
          ba (CellSink. 5)
          out (atom[])
          l (.listen (Cell/apply bf ba) (handler [x] (swap! out conj x)))]
      (.send bf (apply1 [b] (str "12 " b)))
      (.send ba 6)
      (.unlisten l)
      (is (= @out ["1 5" "12 5" "12 6"])))))

(deftest test-lift
  (testing "lift"
    (let [a (CellSink. 1)
          b (CellSink. 5)
          out (atom[])
          l (.listen (Cell/lift (apply2 [x y] (str x " " y)) a b) (handler [x] (swap! out conj x)))]
      (.send a 12)
      (.send b 6)
      (.unlisten l)
      (is (= @out ["1 5" "12 5" "12 6"])))))

(deftest test-lift-glitch
  (testing "lift-glitch"
    (let [a (CellSink. 1)
          a3 (.map a (apply1 [x] (* x 3)))
          a5 (.map a (apply1 [x] (* x 5)))
          b (Cell/lift (apply2 [x y] (str x " " y)) a3 a5)
          out (atom[])
          l (.listen b (handler [x] (swap! out conj x)))]
      (.send a 2)
      (.unlisten l)
      (is (= @out ["3 5" "6 10"])))))

(deftest test-lift-from-simultaneous
  (testing "lift from simultaneous"
    (let [t (Transaction/run (apply0 (let [b1 (CellSink. 3)
                                           b2 (CellSink. 5)]
                                       (.send b2 7)
                                       {:b1 b1 :b2 b2})))
          b1 (:b1 t)
          b2 (:b2 t)
          out (atom[])
          l (.listen (Cell/lift (apply2 [x y] (+ x y)) b1 b2) (handler [x] (swap! out conj x)))]
      (.unlisten l)
      (is (= @out [10])))))

(deftest test-hold-is-delayed
  (testing "hold is delayed"
    (let [e (StreamSink.)
          h (.hold e 0)
          pair (.snapshot e h (apply2 [a b] (str a " " b)))
          out (atom[])
          l (.listen pair (handler [x] (swap! out conj x)))]
      (.send e 2)
      (.send e 3)
      (.unlisten l)
      (is (= @out ["2 0" "3 2"])))))

(definterface ISB)

(deftype SB [a b sw]
  ISB)

(deftest test-switch-c
  (testing "switch c"
    (let [esb (StreamSink.)
          ba (.hold (Stream/filterOptional (.map esb (apply1 [s] (.a s)))) \A)
          bb (.hold (Stream/filterOptional (.map esb (apply1 [s] (.b s)))) \a)
          bsw (.hold (Stream/filterOptional (.map esb (apply1 [s] (.sw s)))) ba)
          bo (Cell/switchC bsw)
          out (atom[])
          l (.listen bo (handler [c] (swap! out conj c)))]
      (.send esb (SB. (oo \B) (oo \b) (oe)))
      (.send esb (SB. (oo \C) (oo \c) (oo bb)))
      (.send esb (SB. (oo \D) (oo \d) (oe)))
      (.send esb (SB. (oo \E) (oo \e) (oo ba)))
      (.send esb (SB. (oo \F) (oo \f) (oe)))
      (.send esb (SB. (oe) (oe) (oo bb)))
      (.send esb (SB. (oe) (oe) (oo ba)))
      (.send esb (SB. (oo \G) (oo \g) (oo bb)))
      (.send esb (SB. (oo \H) (oo \h) (oo ba)))
      (.send esb (SB. (oo \I) (oo \i) (oo ba)))
      (.unlisten l)
      (is (= @out [\A \B \c \d \E \F \f \F \g \H \I])))))

(deftest test-switch-s
  (testing "switch s"
    (let [ese (StreamSink. )
          ea (.map ese (apply1 [s] (.a s)))
          eb (.map ese (apply1 [s] (.b s)))
          bsw (.hold (Stream/filterOptional (.map ese (apply1 [s] (.sw s)))) ea)
          out (atom[])
          eo (Cell/switchS bsw)
          l (.listen eo (handler [c] (swap! out conj c)))]
      (.send ese (SB. \A \a (oe)))
      (.send ese (SB. \B \b (oe)))
      (.send ese (SB. \C \c (oo eb)))
      (.send ese (SB. \D \d (oe)))
      (.send ese (SB. \E \e (oo ea)))
      (.send ese (SB. \F \f (oe)))
      (.send ese (SB. \G \g (oo eb)))
      (.send ese (SB. \H \h (oo ea)))
      (.send ese (SB. \I \i (oo ea)))
      (.unlisten l)
      (is (= @out [\A \B \C \d \e \F \G \h \I])))))

(deftest test-loop-behavior
  (testing "loop behavior"
    (let [ ea (StreamSink.)
          sum_out (Transaction/run (apply0 (let [sum (CellLoop.)
                                                 sum_out_ (.hold (.snapshot ea sum (apply2 [x y] (+ x y))) 0)]
                                             (.loop sum sum_out_)
                                             sum_out_)))
          out (atom[])
          l (.listen sum_out (handler [x] (swap! out conj x)))]
      (.send ea 2)
      (.send ea 3)
      (.send ea 1)
      (.unlisten l)
      (is (= @out [0 2 5 6]))
      (is (= 6 (.sample sum_out))))))

(deftest test-accum
  (testing "accum"
    (let [ea (StreamSink.)
          out (atom[])
          sum (.accum ea 100 (apply2 [a s] (+ a s)))
          l (.listen sum (handler [x] (swap! out conj x)))]
      (.send ea 5)
      (.send ea 7)
      (.send ea 1)
      (.send ea 2)
      (.send ea 3)
      (.unlisten l)
      (is (= @out [100 105 112 113 115 118])))))

(deftest test-loop-value-snapshot
  (testing "loop value snapshot"
    (let [out (atom[])
          l (Transaction/run (apply0 (let [a (Cell. "lettuce")
                                           b (CellLoop.)
                                           eSnap (.snapshot (Operational/value a) b (apply2 [aa bb] (str aa " " bb)))]
                                       (.loop b (Cell. "cheese"))
                                       (.listen eSnap (handler [x] (swap! out conj x))))))]
      (.unlisten l)
      (is (= @out ["lettuce cheese"])))))

(deftest test-loop-value-hold
   (testing "loop value hold"
     (let [out (atom[])
           value (Transaction/run (apply0 (let [a (CellLoop.)
                                                value_ (.hold (Operational/value a) "onion")]
                                            (.loop a (Cell. "cheese"))
                                            value_)))
           eTick (StreamSink.)
           l (.listen (.snapshot eTick value) (handler [x] (swap! out conj x)))]
       (.send eTick Unit/UNIT)
       (.unlisten l)
       (is (= @out ["cheese"])))))

(deftest test-lift-loop
   (testing "lift loop"
     (let [out (atom [])
           b (CellSink. "kettle")
           c (Transaction/run (apply0 (let [a (CellLoop.)
                                            c_ (Cell/lift (apply2 [aa bb] (str aa " " bb)) a b)]
                                        (.loop a (Cell. "tea"))
                                        c_)))
           l (.listen c (handler [x] (swap! out conj x)))]
       (.send b "caddy")
       (.unlisten l)
       (is (= @out ["tea kettle" "tea caddy"])))))

(deftest test-switch-and-defer
   (testing "switch and defer"
     (let [out (atom [])
           si (StreamSink.)
           l (.listen (Cell/switchS
                       (.hold
                        (.map si
                              (apply1 [i] (let [c (Cell. (str "A" i))]
                                            (Operational/defer (Operational/value c)))))
                        (Stream.)))
                      (handler [x] (swap! out conj x)))]
       (.send si 2)
       (.send si 4)
       (.unlisten l)
       (is (= @out ["A2" "A4"])))))
                                           

           







    
            
















