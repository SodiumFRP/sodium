{-# LANGUAGE OverloadedStrings #-}
module Suite where

import Test


base_send1 :: Test
base_send1 = Test "Base" "send1" [
    Transactional [NewStream StringT "s_"] [(StreamSinkT StringT, "s_", s)],
    NewList StringT out,
    Transactional [Listen "l_" s (StringT, "a") [AppendList out (V "a")]] [(ListenerT, "l_", l)],
    Transactional [Send s "a"] [],
    Transactional [Send s "b"] [],
    Unlisten l,
    AssertEqual (List ["a", "b"]) out
   ]
  where
    s = "s"
    out = "out"
    l = "l"

suite :: [Test]
suite = [base_send1]

