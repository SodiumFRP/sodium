{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, DoRec #-}
import FRP.Sodium
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.IORef
import Test.HUnit

event1 = TestCase $ do
    (ev, push) <- sync newEvent
    outRef <- newIORef ""
    sync $ do
        push '?'
    unlisten <- sync $ do
        push 'h'
        unlisten <- listen ev $ \letter -> modifyIORef outRef (++ [letter]) 
        push 'e'
        return unlisten
    sync $ do
        push 'l'
        push 'l'
        push 'o'
    unlisten
    sync $ do
        push '!'
    out <- readIORef outRef
    assertEqual "event1" "hello" =<< readIORef outRef

fmap1 = TestCase $ do
    (ev, push) <- sync newEvent
    outRef <- newIORef ""
    sync $ do
        listen (toUpper `fmap` ev) $ \letter -> modifyIORef outRef (++ [letter])
        push 'h'
        push 'e'
        push 'l'
        push 'l'
        push 'o'
    out <- readIORef outRef
    assertEqual "fmap1" "HELLO" =<< readIORef outRef

merge1 = TestCase $ do
    (ev1, push1) <- sync newEvent
    (ev2, push2) <- sync newEvent
    let ev = merge ev1 ev2
    outRef <- newIORef []
    unlisten <- sync $ listen ev $ \a -> modifyIORef outRef (++ [a])
    sync $ do
        push1 "hello"
        push2 "world"
    sync $ push1 "people"
    sync $ push1 "everywhere"
    unlisten
    assertEqual "merge1" ["hello","world","people","everywhere"] =<< readIORef outRef

filterJust1 = TestCase $ do
    (ema, push) <- sync newEvent
    outRef <- newIORef []
    sync $ do
        listen (filterJust ema) $ \a -> modifyIORef outRef (++ [a])
        push (Just "yes")
        push Nothing
        push (Just "no")
    assertEqual "filterJust1" ["yes", "no"] =<< readIORef outRef

filterE1 = TestCase $ do
    (ec, push) <- sync newEvent
    outRef <- newIORef ""
    sync $ do
        let ed = filterE isDigit ec
        listen ed $ \a -> modifyIORef outRef (++ [a])
        push 'a'
        push '2'
        push 'X'
        push '3'
    assertEqual "filterE1" "23" =<< readIORef outRef

beh1 = TestCase $ do
    outRef <- newIORef []
    (push, unlisten) <- sync $ do
        (beh, push) <- newBehavior "init"
        unlisten <- listenValue beh $ \a -> modifyIORef outRef (++ [a])
        return (push, unlisten)
    sync $ do
        push "next"
    unlisten
    assertEqual "beh1" ["init", "next"] =<< readIORef outRef

beh2 = TestCase $ do
    outRef <- newIORef []
    (push, unlisten) <- sync $ do
        (beh, push) <- newBehavior "init"
        unlisten <- listenValue beh $ \a -> modifyIORef outRef (++ [a])
        return (push, unlisten)
    unlisten
    sync $ do
        push "next"
    assertEqual "beh2" ["init"] =<< readIORef outRef

beh3 = TestCase $ do
    outRef <- newIORef []
    (push, unlisten) <- sync $ do
        (beh, push) <- newBehavior "init"
        unlisten <- listenValue beh $ \a -> modifyIORef outRef (++ [a])
        return (push, unlisten)
    sync $ do
        push "first"
        push "second"
    unlisten
    assertEqual "beh3" ["init", "second"] =<< readIORef outRef

-- | This demonstrates the fact that if there are multiple updates to a behaviour
-- in a given transaction, the last one prevails.
beh4 = TestCase $ do
    outRef <- newIORef []
    (push, unlisten) <- sync $ do
        (beh, push) <- newBehavior "init"
        unlisten <- listenValue beh $ \a -> modifyIORef outRef (++ [a])
        push "other"
        return (push, unlisten)
    sync $ do
        push "first"
        push "second"
    unlisten
    assertEqual "beh4" ["other", "second"] =<< readIORef outRef

beh5 = TestCase $ do
    (ea, push) <- sync newEvent
    outRef <- newIORef []
    unlisten <- sync $ do
        beh <- hold "init" ea
        unlisten <- listen (map toUpper <$> values beh) $ \a -> modifyIORef outRef (++ [a])
        push "other"
        return unlisten
    sync $ do
        push "first"
        push "second"
    unlisten
    assertEqual "beh5" ["OTHER", "SECOND"] =<< readIORef outRef

appl1 = TestCase $ do
    (ea, pusha) <- sync newEvent
    ba <- sync $ hold 0 ea
    (eb, pushb) <- sync newEvent
    bb <- sync $ hold 0 eb
    let esum = (+) <$> ba <*> bb
    outRef <- newIORef []
    unlisten <- sync $ listenValue esum $ \sum -> modifyIORef outRef (++ [sum])
    sync $ pusha 5
    sync $ pushb 100
    sync $ pusha 10 >> pushb 200
    unlisten
    assertEqual "appl1" [0, 5, 105, 210] =<< readIORef outRef

appl2 = TestCase $ do  -- variant that uses listen (valueEvent esum) instead of listenValue
    (ea, pusha) <- sync newEvent
    ba <- sync $ hold 0 ea
    (eb, pushb) <- sync newEvent
    bb <- sync $ hold 0 eb
    let esum = (+) <$> ba <*> bb
    outRef <- newIORef []
    unlisten <- sync $ listen (values esum) $ \sum -> modifyIORef outRef (++ [sum])
    sync $ pusha 5
    sync $ pushb 100
    sync $ pusha 10 >> pushb 200
    unlisten
    assertEqual "appl2" [0, 5, 105, 210] =<< readIORef outRef
    
snapshot1 = TestCase $ do
    (ea, pusha) <- sync newEvent
    (eb, pushb) <- sync newEvent
    bb <- sync $ hold 0 eb
    let ec = snapshotWith (,) ea bb
    outRef <- newIORef []
    unlisten <- sync $ listen ec $ \c -> modifyIORef outRef (++ [c])
    sync $ pusha 'A'
    sync $ pushb 50
    sync $ pusha 'B'
    sync $ pusha 'C' >> pushb 60
    sync $ pusha 'D'
    unlisten
    assertEqual "snapshot1" [('A',0),('B',50),('C',50),('D',60)] =<< readIORef outRef

count1 = TestCase $ do
    (ea, push) <- sync newEvent
    outRef <- newIORef []
    unlisten <- sync $ do
        eCount <- countE ea
        listen eCount $ \c -> modifyIORef outRef (++ [c])
    sync $ push ()
    sync $ push ()
    sync $ push ()
    unlisten
    assertEqual "count1" [1,2,3] =<< readIORef outRef

collect1 = TestCase $ do
    (ea, push) <- sync newEvent
    outRef <- newIORef []
    unlisten <- sync $ do
        ba <- hold 100 ea
        sum <- collect (\a s -> (a+s, a+s)) 0 ba
        listenValue sum $ \sum -> modifyIORef outRef (++ [sum])
    sync $ push 5
    sync $ push 7
    sync $ push 1
    sync $ push 2
    sync $ push 3
    unlisten
    assertEqual "collect1" [100, 105, 112, 113, 115, 118] =<< readIORef outRef

collect2 = TestCase $ do
    outRef <- newIORef []
    -- This is a bit of an edge case.
    (unlisten, push) <- sync $ do
        (ba, push) <- newBehavior 100
        sum <- collect (\a s -> (a + s, a + s)) 0 ba
        push 5
        unlisten <- listenValue sum $ \sum -> modifyIORef outRef (++ [sum])
        return (unlisten, push)
    sync $ push 7
    sync $ push 1
    unlisten
    assertEqual "collect2" [100, 105, 112, 113] =<< readIORef outRef

collectE1 = TestCase $ do
    (ea, push) <- sync newEvent
    outRef <- newIORef []
    unlisten <- sync $ do
        sum <- collectE (\a s -> (a+s, a+s)) 100 ea
        listen sum $ \sum -> modifyIORef outRef (++ [sum])
    sync $ push 5
    sync $ push 7
    sync $ push 1
    sync $ push 2
    sync $ push 3
    unlisten
    assertEqual "collectE1" [105, 112, 113, 115, 118] =<< readIORef outRef

collectE2 = TestCase $ do
    (ea, push) <- sync newEvent
    outRef <- newIORef []
    -- This behaviour is a little bit odd but difficult to fix in the
    -- implementation. However, it shouldn't be too much of a problem in
    -- practice. Here we are defining it.
    unlisten <- sync $ do
        sum <- collectE (\a s -> (a + s, a + s)) 100 ea
        push 5
        listen sum $ \sum -> modifyIORef outRef (++ [sum])
    sync $ push 7
    sync $ push 1
    unlisten
    assertEqual "collectE2" [105, 112, 113] =<< readIORef outRef

switchE1 = TestCase $ do
    (ea, pusha) <- sync newEvent
    (eb, pushb) <- sync newEvent
    (esw, pushsw) <- sync newEvent
    outRef <- newIORef []
    unlisten <- sync $ do
        sw <- hold ea esw
        let eo = switchE sw
        unlisten <- listen eo $ \o -> modifyIORef outRef (++ [o])
        pusha 'A'
        pushb 'a'
        return unlisten
    sync $ pusha 'B' >> pushb 'b'
    sync $ pushsw eb >> pusha 'C' >> pushb 'c'
    sync $ pusha 'D' >> pushb 'd'
    sync $ pusha 'E' >> pushb 'e' >> pushsw ea
    sync $ pusha 'F' >> pushb 'f'
    sync $ pusha 'G' >> pushb 'g' >> pushsw eb
    sync $ pusha 'H' >> pushb 'h' >> pushsw ea
    sync $ pusha 'I' >> pushb 'i' >> pushsw ea
    unlisten
    assertEqual "switchE1" "ABCdeFGhI" =<< readIORef outRef

switch1 = TestCase $ do
    (ea, pusha) <- sync newEvent
    (eb, pushb) <- sync newEvent
    (esw, pushsw) <- sync newEvent
    outRef <- newIORef []
    (ba, bb, unlisten) <- sync $ do
        ba <- hold 'A' ea
        bb <- hold 'a' eb
        bsw <- hold ba esw
        bo <- switch bsw
        unlisten <- listenValue bo $ \o -> modifyIORef outRef (++ [o])
        return (ba, bb, unlisten)
    sync $ pusha 'B' >> pushb 'b'
    sync $ pushsw bb >> pusha 'C' >> pushb 'c'
    sync $ pusha 'D' >> pushb 'd'
    sync $ pusha 'E' >> pushb 'e' >> pushsw ba
    sync $ pusha 'F' >> pushb 'f'
    sync $ pushsw bb
    sync $ pushsw ba
    sync $ pusha 'G' >> pushb 'g' >> pushsw bb
    sync $ pusha 'H' >> pushb 'h' >> pushsw ba
    sync $ pusha 'I' >> pushb 'i' >> pushsw ba
    unlisten
    assertEqual "switch1" "ABcdEFfFgHI" =<< readIORef outRef
    
once1 = TestCase $ do
    (ea, pusha) <- sync newEvent
    outRef <- newIORef []
    unlisten <- sync $ do
        oea <- once ea
        listen oea $ \a -> modifyIORef outRef (++ [a])
    sync $ pusha 'A'
    sync $ pusha 'B'
    sync $ pusha 'C'
    unlisten
    assertEqual "switch1" "A" =<< readIORef outRef

once2 = TestCase $ do
    (ea, pusha) <- sync newEvent
    outRef <- newIORef []
    unlisten <- sync $ do
        oea <- once ea
        pusha 'A'
        listen oea $ \a -> modifyIORef outRef (++ [a])
    sync $ pusha 'B'
    sync $ pusha 'C'
    unlisten
    assertEqual "switch1" "A" =<< readIORef outRef

{-
crossE1 = TestCase $ do
    outRef <- newIORef []
    (ema :: Event Plain Char, push) <- newEvent
    (ena :: Event N Char) <- sync $ crossE ema
    unlisten <- sync $ listen ena $ \a -> modifyIORef outRef (++ [a])
    sync $ push 'A'
    sync $ push 'M'
    sync $ push 'T'
    -- Flush processing on partition N before unlistening
    sync (return () :: Reactive N ())
    unlisten
    assertEqual "crossE1" "AMT" =<< readIORef outRef

cross1 = TestCase $ do
    outRef <- newIORef []
    (ema :: Event Plain Char, push) <- newEvent
    bma <- sync $ hold 'A' ema
    sync $ push 'B'
    (bna :: Behavior N Char) <- sync $ cross bma
    unlisten <- sync $ listenValue bna $ \a -> modifyIORef outRef (++ [a])
    sync $ push 'C'
    sync $ push 'D'
    sync $ push 'E'
    -- Flush processing on partition N before unlistening
    sync (return () :: Reactive N ())
    unlisten
    assertEqual "cross1" "BCDE" =<< readIORef outRef

cross2 = TestCase $ do
    outRef <- newIORef []
    (ema :: Event Plain Char, push) <- newEvent
    bma <- sync $ hold 'A' ema
    (bna :: Behavior N Char) <- sync $ cross bma
    unlisten <- sync $ listenValue bna $ \a -> modifyIORef outRef (++ [a])
    sync $ push 'B'
    sync $ push 'C'
    sync $ push 'D'
    sync $ push 'E'
    -- Flush processing on partition N before unlistening
    sync (return () :: Reactive N ())
    unlisten
    assertEqual "cross1" "ABCDE" =<< readIORef outRef
-}

data Page = Page { unPage :: Reactive (Char, Event Page) }

cycle1 = TestCase $ do
    outRef <- newIORef []
    (ep, push) <- sync newEvent
    bo <- sync $ do
        let initPair = ('a', ep)
        rec
            bPair <- hold initPair ePage
            let ePage = execute $ unPage <$> switchE (snd <$> bPair)
        return (fst <$> bPair)
    unlisten <- sync $ listenValue bo $ \o -> modifyIORef outRef (++ [o])
    sync $ push (Page $ return ('b', ep))
    sync $ push (Page $ return ('c', ep))
    unlisten
    assertEqual "cycle1" "abc" =<< readIORef outRef

mergeWith1 = TestCase $ do
    outRef <- newIORef []
    (ea, pushA) <- sync newEvent
    (eb, pushB) <- sync newEvent
    unlisten <- sync $ do
        pushA 5
        listen (mergeWith (+) ea eb) $ \o -> modifyIORef outRef (++ [o])
    sync $ pushA 2
    sync $ pushB 3
    sync $ pushA 10 >> pushB 4
    sync $ pushB 7 >> pushA 1
    unlisten
    assertEqual "mergeWith1" [5,2,3,14,8] =<< readIORef outRef

mergeWith2 = TestCase $ do
    outRef <- newIORef []
    (ea, pushA) <- sync newEvent
    (eb, pushB) <- sync newEvent
    unlisten <- sync $ do
        pushA 5
        unlisten <- listen (mergeWith (+) ea eb) $ \o -> modifyIORef outRef (++ [o])
        pushB 99
        return unlisten
    unlisten
    assertEqual "mergeWith2" [104] =<< readIORef outRef

mergeWith3 = TestCase $ do
    outRef <- newIORef []
    (ea, pushA) <- sync newEvent
    (eb, pushB) <- sync newEvent
    unlisten <- sync $ do
        listen (mergeWith (+) ea eb) $ \o -> modifyIORef outRef (++ [o])
    sync $ pushA 2
    sync $ pushB 3 >> pushB 1 >> pushA 10
    sync $ pushB 9 >> pushB 11 >> pushB 12
    sync $ pushA 32 >> pushA 11 >> pushA 12
    unlisten
    assertEqual "mergeWith3" [2,14,32,55] =<< readIORef outRef

coalesce1 = TestCase $ do
    outRef <- newIORef []
    (ea, pushA) <- sync newEvent
    (eb, pushB) <- sync newEvent
    unlisten <- sync $ do
        listen (coalesce (+) (merge ea eb)) $ \o -> modifyIORef outRef (++ [o])
    sync $ pushA 2
    sync $ pushA 5 >> pushB 6
    unlisten
    assertEqual "coalesce1" [2, 11] =<< readIORef outRef

tests = test [ event1, fmap1, merge1, filterJust1, filterE1, beh1, beh2, beh3, beh4, beh5,
    appl1, appl2, snapshot1, count1, collect1, collect2, collectE1, collectE2, switchE1,
    switch1, once1, once2, {-crossE1, cross1, cross2,-} cycle1, mergeWith1, mergeWith2, mergeWith3,
    coalesce1 ]

main = {-forever $-} runTestTT tests

