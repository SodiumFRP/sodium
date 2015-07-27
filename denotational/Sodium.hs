{-# LANGUAGE GADTs #-}
-- Denotational Semantics of Sodium 1.0.
-- Executable version.
-- Please see the accompanying text document.
import Test.HUnit
import Control.Applicative

type T = [Int]

type S a = [(T, a)]         -- for increasing T values
type C a = (a, [(T, a)])    -- for increasing T values

data Stream a where
    MkStream  :: S a -> Stream a
    Never    :: Stream a
    MapS     :: (a -> b) -> Stream a -> Stream b
    Snapshot :: (a -> b -> c) -> Stream a -> Cell b -> Stream c
    Merge    :: Stream a -> Stream a -> (a -> a -> a) -> Stream a
    Filter   :: (a -> Bool) -> Stream a -> Stream a
    SwitchS  :: Cell (Stream a) -> Stream a
    Execute  :: Stream (Reactive a) -> Stream a
    Updates  :: Cell a -> Stream a
    Value    :: Cell a -> T -> Stream a
    Split    :: Stream [a] -> Stream a

at :: C a -> T -> a
at (a, sts) t = last (a : map snd (filter (\(tt, a) -> tt < t) sts))

chopFront :: C a -> T -> C a
chopFront (i, sts) t0 = (at (i, sts) t0, filter (\(t, a) -> t >= t0) sts)

occs :: Stream a -> S a
occs (MkStream s) = s
occs Never = []
occs (MapS f s) = map (\(t, a) -> (t, f a)) (occs s)
occs (Snapshot f s c) = map (\(t, a) -> (t, f a (at stsb t))) (occs s)
  where stsb = steps c
occs (Merge sa sb f) = coalesce f (knit (occs sa) (occs sb))
  where knit ((ta, a):as) bs@((tb, _):_) | ta <= tb = (ta, a) : knit as bs
        knit as@((ta, _):_) ((tb, b):bs) = (tb, b) : knit as bs
        knit as bs = as ++ bs
occs (Filter pred s) = filter (\(t, a) -> pred a) (occs s)
occs (SwitchS c) = scan Nothing a sts
  where (a, sts) = steps c
        scan mt0 a0 ((t1, a1):as) =
            filter (\(t, a) -> maybe True (t >) mt0 && t <= t1) (occs a0)
            ++ scan (Just t1) a1 as
        scan mt0 a0 [] =
            filter (\(t, a) -> maybe True (t >) mt0) (occs a0)
occs (Execute s) = map (\(t, ma) -> (t, run ma t)) (occs s)
occs (Updates c) = sts
  where (_, sts) = steps c
occs (Value c t0) = coalesce (flip const) ((t0, a) : sts)
  where (a, sts) = chopFront (steps c) t0
occs (Split s) = concatMap split (coalesce (++) (occs s))
  where split (t, as) = zipWith (\n a -> (t++[n], a)) [0..] as

coalesce :: (a -> a -> a) -> S a -> S a
coalesce f ((t1, a1):(t2, a2):as) | t1 == t2 = coalesce f ((t1, f a1 a2):as)
coalesce f (ta:as) = ta : coalesce f as
coalesce f [] = []

data Cell a where
    Constant :: a -> Cell a
    Hold     :: a -> Stream a -> T -> Cell a
    MapC     :: (a -> b) -> Cell a -> Cell b
    Apply    :: Cell (a -> b) -> Cell a -> Cell b
    SwitchC  :: Cell (Cell a) -> T -> Cell a

steps :: Cell a -> C a
steps (Constant a) = (a, [])
steps (Hold a s t0) = (a, coalesce (flip const)
    (filter (\(t, a) -> t >= t0) (occs s)))
steps (MapC f c) = (f a, map (\(t, a) -> (t, f a)) sts)
    where (a, sts) = steps c
steps (Apply cf ca) = (f a, knit f fsts a asts)
    where (f, fsts) = steps cf
          (a, asts) = steps ca
          knit _ ((tf, f):fs) a as@((ta, _):_) | tf < ta = (tf, f a) : knit f fs a as
          knit f fs@((tf, _):_) _ ((ta, a):as) | tf > ta = (ta, f a) : knit f fs a as
          knit _ ((tf, f):fs) _ ((ta, a):as) | tf == ta = (tf, f a) : knit f fs a as 
          knit _ ((tf, f):fs) a [] = (tf, f a) : knit f fs a []
          knit f [] _ ((ta, a):as) = (ta, f a) : knit f [] a as
          knit _ [] _ [] = []
steps (SwitchC c t0) = (at (steps (at (steps c) t0)) t0,
        coalesce (flip const) (scan t0 a sts))
    where (a, sts) = steps c
          scan t0 a0 ((t1, a1):as) =
              let (b, stsb) = normalize (chopBack (chopFront (steps a0) t0) t1)
              in  ((t0, b) : stsb) ++ scan t1 a1 as
          scan t0 a0 [] =
              let (b, stsb) = normalize (chopFront (steps a0) t0)
              in  ((t0, b) : stsb)
          normalize :: C a -> C a
          normalize (_, (t1, a) : as) | t1 == t0 = (a, as)
          normalize as = as
          chopBack :: C a -> T -> C a
          chopBack (i, sts) tEnd = (i, filter (\(t, a) -> t < tEnd) sts)

data Reactive a = Reactive { run :: T -> a }

instance Functor Reactive where
    fmap f (Reactive ra) = Reactive $ \t -> f (ra t)

instance Applicative Reactive where
    pure a = Reactive $ \_ -> a
    (Reactive rf) <*> (Reactive ra) = Reactive $ \t -> rf t (ra t)

instance Monad Reactive where
    (Reactive ra) >>= arb = Reactive $ \t -> run (arb (ra t)) t

hold :: a -> Stream a -> Reactive (Cell a)
hold a s = Reactive (Hold a s)

sample :: Cell a -> Reactive a
sample c = Reactive (at (steps c))

value :: Cell a -> Reactive (Stream a)
value c = Reactive (Value c)

-- Defined in terms of MapS, Sample and Execute.
snapshot2 :: (a -> b -> c) -> Stream a -> Cell b -> Stream c
snapshot2 f s c = Execute (MapS (\a -> f a <$> sample c) s)

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
        let s1 = MkStream [([0], ['a', 'b']), ([1],['c']),([1],['d','e'])]
        let s2 = Split s1
        assertEqual "s2" [([0,0],'a'),([0,1],'b'),([1,0],'c'),([1,1],'d'),([1,2],'e')] (occs s2),
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

