{-# LANGUAGE GADTs #-}
module Reactive.Sodium.Denotational where
-- Denotational Semantics of Sodium 1.1.
-- Executable version.
-- Please see the accompanying text document.

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
