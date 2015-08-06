{-# LANGUAGE OverloadedStrings #-}
module OperationalTests where

import Type
import Operational


base_send1 :: Test
base_send1 = Test "Base" "send1" [
    Transactional [NewStream StringT "s_"] [(StreamSinkT StringT, "s_", s)],
    NewList StringT out,
    Transactional [Listen "l_" s (StringT, "a") [AppendList out (V "a")]] [(ListenerT, "l_", l)],
    Transactional [Send s "a"] [],
    Transactional [Send s "b"] [],
    Unlisten l,
    AssertEqual (lit ["a", "b" :: String]) out
   ]
  where
    s = "s"
    out = "out"
    l = "l"

operationalTests :: [Test]
operationalTests =
    [base_send1]

