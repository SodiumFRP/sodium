(ns sodium.common-test
  (:require [clojure.test :refer :all]
            [sodium.core :refer :all])
  (:import [nz.sodium])
  (:import [nz.sodium Operational Stream StreamSink StreamLoop CellSink Cell Transaction])
  (:import [java.util ArrayList]))

(deftest test-base-send
  (testing "base send"
    (let [s (Transaction/run (apply0
                              (let [s_ (StreamSink.)]
                                s_)))
          out (atom [])
          l (Transaction/run (apply0
                              (let [l_ (.listen s (handler [s] (swap! out conj s)))]
                                l_)))]
      (Transaction/runVoid #(.send s "a"))
      (Transaction/runVoid #(.send s "b"))
      (.unlisten l)
      (is (= @out ["a" "b"])))))

(deftest test-operational-split
  (testing "operational split"
    (let [a (Transaction/run (apply0
                              (let [a_ (StreamSink.)]
                                a_)))
          b (Transaction/run (apply0
                              (let [b_ (Operational/split a)]
                                b_)))
          b_0 (atom [])
          b_0_l (Transaction/run (apply0
                                  (let [b_0_l_ (.listen b (handler [val] (swap! b_0 conj val)))]
                                    b_0_l_)))]
      (Transaction/runVoid #(.send a ["a" "b"]))
      (.unlisten b_0_l)
      (is (= @b_0 ["a" "b"])))))

(deftest test-operational-defer-1
  (testing "operational defer 1"
    (let [a (Transaction/run (apply0
                              (let [a_ (StreamSink.)]
                                a_)))
          b (Transaction/run (apply0
                              (let [b_ (Operational/defer a)]
                                b_)))
          b_0 (atom [])
          b_0_l (Transaction/run (apply0
                                  (let [b_0_l_ (.listen b (handler [val] (swap! b_0 conj val)))]
                                    b_0_l_)))]
      (Transaction/runVoid #(.send a "a"))
      (.unlisten b_0_l)
      (is (= @b_0 ["a"]))
      (let [b_1 (atom [])
            b_1_l (Transaction/run (apply0
                                    (let [b_1_l_ (.listen b (handler [val] (swap! b_1 conj val)))]
                                      b_1_l_)))]
        (Transaction/runVoid #(.send a "b"))
        (.unlisten b_1_l)
        (is (= @b_1 ["b"]))))))
          
(deftest test-operational-defer-2
  (testing "operational defer 1"
    (let [a (Transaction/run (apply0
                              (let [a_ (StreamSink.)]
                                a_)))
          b (Transaction/run (apply0
                              (let [b_ (StreamSink.)]
                                b_)))
          c (Transaction/run (apply0
                              (let [c_ (.orElse (Operational/defer a) b)]
                                c_)))
          c_0 (atom [])
          c_0_l (Transaction/run (apply0
                                  (let [c_0_l_ (.listen c (handler [val] (swap! c_0 conj val)))]
                                    c_0_l_)))]
      (Transaction/runVoid #(.send a "a"))
      (.unlisten c_0_l)
      (is (= @c_0 ["a"]))
      (let [c_1 (atom [])
            c_1_l (Transaction/run (apply0
                                    (let [c_1_l_ (.listen c (handler [val] (swap! c_1 conj val)))]
                                      c_1_l_)))]
        (Transaction/runVoid #(do
                                (.send a "b")
                                (.send b "B")))
        (.unlisten c_1_l)
        (is (= @c_1 ["B" "b"]))))))

(deftest test-stream-or-else-1
  (testing "stream or else 1"
    (let [a (Transaction/run (apply0
                              (let [a_ (StreamSink.)]
                                a_)))
          b (Transaction/run (apply0
                              (let [b_ (StreamSink.)]
                              b_)))
          c (Transaction/run (apply0
                              (let [c_ (.orElse a b)]
                                c_)))
          c_0 (atom [])
          c_0_l (Transaction/run (apply0
                                  (let [c_0_l_ (.listen c (handler [val] (swap! c_0 conj val)))]
                                    c_0_l_)))]
          (Transaction/runVoid #(.send a 0))
          (.unlisten c_0_l)
          (is (= @c_0 [0]))
          (let [c_1 (atom [])
                c_1_l (Transaction/run (apply0
                                        (let [c_1_l_ (.listen c (handler [val] (swap! c_1 conj val)))]
                                          c_1_l_)))]
            (Transaction/runVoid #(.send b 10))
            (.unlisten c_1_l)
            (is (= @c_1 [10])))
          (let [c_2 (atom [])
                c_2_l (Transaction/run (apply0
                                        (let [c_2_l_ (.listen c (handler [val] (swap! c_2 conj val)))]
                                          c_2_l_)))]
            (Transaction/runVoid #(do
                                    (.send a 2)
                                    (.send b 20)))
            (.unlisten c_2_l)
            (is (= @c_2 [2])))
          (let [c_3 (atom [])
                c_3_l (Transaction/run (apply0
                                        (let [c_3_l_ (.listen c (handler [val] (swap! c_3 conj val)))]
                                          c_3_l_)))]
            (Transaction/runVoid #(.send b 30))
            (.unlisten c_3_l)
            (is (= @c_3 [30]))))))

(deftest test-operational-defer-simultaneous
  (testing "operational defer simultaneous"
    (let [a (Transaction/run (apply0
                              (let [a_ (StreamSink.)]
                                a_)))
          b (Transaction/run (apply0
                              (let [b_ (StreamSink.)]
                              b_)))
          c (Transaction/run (apply0
                              (let [c_ (.orElse (Operational/defer a) (Operational/defer b))]
                                c_)))
          c_0 (atom [])
          c_0_l (Transaction/run (apply0
                                  (let [c_0_l_ (.listen c (handler [val] (swap! c_0 conj val)))]
                                    c_0_l_)))]
      (Transaction/runVoid #(.send b "A"))
      (.unlisten c_0_l)
      (is (= @c_0 ["A"]))
      (let [c_1 (atom [])
            c_1_l (Transaction/run (apply0
                                    (let [c_1_l_ (.listen c (handler [val] (swap! c_1 conj val)))]
                                      c_1_l_)))]
        (Transaction/runVoid #(do
                                (.send a "b")
                                (.send b "B")))
        (.unlisten c_1_l)
        (is (= @c_1 ["b"]))))))
                  

            
                
                
          
          

