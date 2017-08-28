{-# LANGUAGE GADTs #-}
-- Denotational Semantics of Sodium 1.1.
-- Executable version.
-- Please see the accompanying text document.
import Test.HUnit
import Reactive.Sodium.Denotational


main :: IO ()
main = do
    runTestTT tests
    return ()

tests :: Test
tests = test [
    "Never" ~: do
        let s = Never :: Stream ()
        assertEqual "e" [] (occs s),
    "MapS" ~: do
        let s1 = MkStream [([0], 5), ([1], 10), ([2], 12)]
        let s2 = MapS (1+) s1
        assertEqual "s2" [([0],6),([1],11),([2],13)] (occs s2),
    "Snapshot" ~: do
        let c = Hold 3 (MkStream [([1], 4), ([5], 7)]) [0]
        let s1 = MkStream [([0], 'a'), ([3], 'b'), ([5], 'c')]
        let s2 = Snapshot (flip const) s1 c
        assertEqual "s2" [([0],3),([3],4),([5],4)] (occs s2)
        let s3 = snapshot2 (flip const) s1 c
        assertEqual "s3" (occs s2) (occs s3),
    "Merge" ~: do
        let s1 = MkStream [([0], 0), ([2], 2)]
        let s2 = MkStream [([1], 10), ([2], 20), ([3], 30)]
        let s3 = Merge s1 s2 (+)
        assertEqual "s3" [([0],0),([1],10),([2],22),([3],30)] (occs s3),
    "Filter" ~: do
        let s1 = MkStream [([0], 5), ([1], 6), ([2], 7)]
        let s2 = Filter odd s1
        assertEqual "s2" [([0],5),([2],7)] (occs s2),
    "SwitchS" ~: do
        let s1 = MkStream [([0], 'a'), ([1], 'b'), ([2], 'c'), ([3], 'd')]
        let s2 = MkStream [([0], 'W'), ([1], 'X'), ([2], 'Y'), ([3], 'Z')]
        let c = Hold s1 (MkStream [([1], s2)]) [0]
        let s3 = SwitchS c
        assertEqual "s3" [([0],'a'),([1],'b'),([2],'Y'),([3],'Z')] (occs s3),
    "Execute" ~: do
        let s1 = MkStream [([0], return 'a')]
        let s2 = Execute s1
        assertEqual "s2" [([0],'a')] (occs s2),
    "Updates" ~: do
        let c = Hold 'a' (MkStream [([1], 'b'), ([3], 'c')]) [0]
        let s = Updates c
        assertEqual "s" [([1],'b'),([3],'c')] (occs s),
    "Value 1" ~: do
        let c = Hold 'a' (MkStream [([1], 'b'), ([3], 'c')]) [0]
        let s = Value c [0]
        assertEqual "s" [([0],'a'),([1],'b'),([3],'c')] (occs s),
    "Value 2" ~: do
        let c = Hold 'a' (MkStream [([0], 'b'), ([1], 'c'), ([3], 'd')]) [0]
        let s = Value c [0]
        assertEqual "s" [([0],'b'),([1],'c'),([3],'d')] (occs s),
    "Split" ~: do
        let s1 = MkStream [([0], ['a', 'b']), ([1],['c'])]
        let s2 = Split s1
        assertEqual "s2" [([0,0],'a'),([0,1],'b'),([1,0],'c')] (occs s2),
    "Constant" ~: do
        let c = Constant 'a'
        assertEqual "c" ('a',[]) (steps c),
    "Hold" ~: do
        let c = Hold 'a' (MkStream [([1], 'b'), ([3], 'c')]) [0]
        assertEqual "b" ('a',[([1],'b'),([3],'c')]) (steps c),
    "MapC" ~: do
        let c1 = Hold 0 (MkStream [([2], 3), ([3], 5)]) [0]
        let c2 = MapC (1+) c1
        assertEqual "c2" (1,[([2],4),([3],6)]) (steps c2),
    "Apply" ~: do
        let cf = Hold (0+) (MkStream [([1], (5+)), ([3], (6+))]) [0]
        let ca = Hold (100 :: Int) (MkStream [([1], 200), ([2], 300), ([4], 400)]) [0]
        let cb = Apply cf ca
        assertEqual "cb" (100,[([1],205),([2],305),([3],306),([4],406)]) (steps cb),
    "SwitchC 1" ~: do
        let c1 = Hold 'a' (MkStream [([0], 'b'), ([1], 'c'), ([2], 'd'), ([3], 'e')]) [0]
        let c2 = Hold 'V' (MkStream [([0], 'W'), ([1], 'X'), ([2], 'Y'), ([3], 'Z')]) [0]
        let c3 = Hold c1 (MkStream [([1], c2)]) [0]
        let c4 = SwitchC c3 [0]
        assertEqual "c4" ('a',[([0],'b'),([1],'X'),([2],'Y'),([3],'Z')]) (steps c4),
    "SwitchC 2" ~: do
        let c1 = Hold 'a' (MkStream [([0], 'b'), ([1], 'c'), ([2], 'd'), ([3], 'e')]) [0]
        let c2 = Hold 'W' (MkStream [([1], 'X'), ([2], 'Y'), ([3], 'Z')]) [0]
        let c3 = Hold c1 (MkStream [([1], c2)]) [0]
        let c4 = SwitchC c3 [0]
        assertEqual "c4" ('a',[([0],'b'),([1],'X'),([2],'Y'),([3],'Z')]) (steps c4),
    "SwitchC 3" ~: do
        let c1 = Hold 'a' (MkStream [([0], 'b'), ([1], 'c'), ([2], 'd'), ([3], 'e')]) [0]
        let c2 = Hold 'X' (MkStream [([2], 'Y'), ([3], 'Z')]) [0]
        let c3 = Hold c1 (MkStream [([1], c2)]) [0]
        let c4 = SwitchC c3 [0]
        assertEqual "c4" ('a',[([0],'b'),([1],'X'),([2],'Y'),([3],'Z')]) (steps c4),
    "SwitchC 4" ~: do
        let c1 = Hold 'a' (MkStream [([0], 'b'), ([1], 'c'), ([2], 'd'), ([3], 'e')]) [0]
        let c2 = Hold 'V' (MkStream [([0], 'W'), ([1], 'X'), ([2], 'Y'), ([3], 'Z')]) [0]
        let c3 = Hold '1' (MkStream [([0], '2'), ([1], '3'), ([2], '4'), ([3], '5')]) [0]
        let c4 = Hold c1 (MkStream [([1], c2), ([3], c3)]) [0]
        let c5 = SwitchC c4 [0]
        assertEqual "c5" ('a',[([0],'b'),([1],'X'),([2],'Y'),([3],'5')]) (steps c5),
    "Sample" ~: do
        let c = Hold 'a' (MkStream [([1], 'b')]) [0]
        let a1 = run (sample c) [1]
        assertEqual "a" 'a' a1
        let a2 = run (sample c) [2]
        assertEqual "a" 'b' a2
        
  ]

