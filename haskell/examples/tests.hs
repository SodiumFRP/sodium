{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, EmptyDataDecls, DoRec #-}
import FRP.Sodium
import FRP.Sodium.Internal
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.IORef
import Data.Typeable
import Test.HUnit

data M deriving Typeable  -- Define a main partition
data N deriving Typeable  -- Define a secondary partition

event1 = TestCase $ do
    (ev :: Event M Char, push) <- newEvent
    outRef <- newIORef ""
    synchronously $ do
        push '?'
    unlisten <- synchronously $ do
        push 'h'
        unlisten <- listenIO ev $ \letter -> modifyIORef outRef (++ [letter]) 
        push 'e'
        return unlisten
    synchronously $ do
        push 'l'
        push 'l'
        push 'o'
    unlisten
    synchronously $ do
        push '!'
    out <- readIORef outRef
    assertEqual "event1" "hello" =<< readIORef outRef

fmap1 = TestCase $ do
    (ev :: Event M Char, push) <- newEvent
    outRef <- newIORef ""
    synchronously $ do
        listenIO (toUpper `fmap` ev) $ \letter -> modifyIORef outRef (++ [letter])
        push 'h'
        push 'e'
        push 'l'
        push 'l'
        push 'o'
    out <- readIORef outRef
    assertEqual "fmap1" "HELLO" =<< readIORef outRef

merge1 = TestCase $ do
    (ev1 :: Event M String, push1) <- newEvent
    (ev2, push2) <- newEvent
    let ev = merge ev1 ev2
    outRef <- newIORef []
    unlisten <- synchronously $ listenIO ev $ \a -> modifyIORef outRef (++ [a])
    synchronously $ do
        push1 "hello"
        push2 "world"
    synchronously $ push1 "people"
    synchronously $ push1 "everywhere"
    unlisten
    assertEqual "merge1" ["hello","world","people","everywhere"] =<< readIORef outRef

filterJust1 = TestCase $ do
    (ema :: Event M (Maybe String), push) <- newEvent
    outRef <- newIORef []
    synchronously $ do
        listenIO (filterJust ema) $ \a -> modifyIORef outRef (++ [a])
        push (Just "yes")
        push Nothing
        push (Just "no")
    assertEqual "filterJust1" ["yes", "no"] =<< readIORef outRef

filterE1 = TestCase $ do
    (ec, push) <- newEvent
    outRef <- newIORef ""
    synchronously $ do
        let ed = filterE isDigit (ec :: Event M Char)
        listenIO ed $ \a -> modifyIORef outRef (++ [a])
        push 'a'
        push '2'
        push 'X'
        push '3'
    assertEqual "filterE1" "23" =<< readIORef outRef

beh1 = TestCase $ do
    (ea :: Event M String, push) <- newEvent
    outRef <- newIORef []
    unlisten <- synchronously $ do
        beh <- hold "init" ea
        listenValueIO beh $ \a -> modifyIORef outRef (++ [a])
    synchronously $ do
        push "next"
    unlisten
    assertEqual "beh1" ["init", "next"] =<< readIORef outRef

beh2 = TestCase $ do
    (ea :: Event M String, push) <- newEvent
    outRef <- newIORef []
    unlisten <- synchronously $ do
        beh <- hold "init" ea
        listenValueIO beh $ \a -> modifyIORef outRef (++ [a])
    unlisten
    synchronously $ do
        push "next"
    assertEqual "beh2" ["init"] =<< readIORef outRef

beh3 = TestCase $ do
    (ea :: Event M String, push) <- newEvent
    outRef <- newIORef []
    unlisten <- synchronously $ do
        beh <- hold "init" ea
        listenValueIO beh $ \a -> modifyIORef outRef (++ [a])
    synchronously $ do
        push "first"
        push "second"
    unlisten
    assertEqual "beh3" ["init", "second"] =<< readIORef outRef

-- | This demonstrates the fact that if there are multiple updates to a behaviour
-- in a given transaction, the last one prevails.
beh4 = TestCase $ do
    (ea :: Event M String, push) <- newEvent
    outRef <- newIORef []
    unlisten <- synchronously $ do
        beh <- hold "init" ea
        unlisten <- listenValueIO beh $ \a -> modifyIORef outRef (++ [a])
        push "other"
        return unlisten
    synchronously $ do
        push "first"
        push "second"
    unlisten
    assertEqual "beh4" ["other", "second"] =<< readIORef outRef

beh5 = TestCase $ do
    (ea :: Event M String, push) <- newEvent
    outRef <- newIORef []
    unlisten <- synchronously $ do
        beh <- hold "init" ea
        unlisten <- listenIO (valueEvent beh) $ \a -> modifyIORef outRef (++ [a])
        push "other"
        return unlisten
    synchronously $ do
        push "first"
        push "second"
    unlisten
    assertEqual "beh5" ["other", "second"] =<< readIORef outRef

beh6 = TestCase $ do
    (ea :: Event M String, push) <- newEvent
    outRef <- newIORef []
    unlisten <- synchronously $ do
        beh <- hold "init" ea
        unlisten <- listenIO (map toUpper <$> valueEvent beh) $ \a -> modifyIORef outRef (++ [a])
        push "other"
        return unlisten
    synchronously $ do
        push "first"
        push "second"
    unlisten
    assertEqual "beh6" ["OTHER", "SECOND"] =<< readIORef outRef

appl1 = TestCase $ do
    (ea :: Event M Int, pusha) <- newEvent
    ba <- synchronously $ hold 0 ea
    (eb, pushb) <- newEvent
    bb <- synchronously $ hold 0 eb
    let esum = (+) <$> ba <*> bb
    outRef <- newIORef []
    unlisten <- synchronously $ listenValueIO esum $ \sum -> modifyIORef outRef (++ [sum])
    synchronously $ pusha 5
    synchronously $ pushb 100
    synchronously $ pusha 10 >> pushb 200
    unlisten
    assertEqual "appl1" [0, 5, 105, 210] =<< readIORef outRef

appl2 = TestCase $ do  -- variant that uses listenIO (valueEvent esum) instead of listenValueIO
    (ea :: Event M Int, pusha) <- newEvent
    ba <- synchronously $ hold 0 ea
    (eb, pushb) <- newEvent
    bb <- synchronously $ hold 0 eb
    let esum = (+) <$> ba <*> bb
    outRef <- newIORef []
    unlisten <- synchronously $ listenIO (valueEvent esum) $ \sum -> modifyIORef outRef (++ [sum])
    synchronously $ pusha 5
    synchronously $ pushb 100
    synchronously $ pusha 10 >> pushb 200
    unlisten
    assertEqual "appl2" [0, 5, 105, 210] =<< readIORef outRef
    
snapshot1 = TestCase $ do
    (ea :: Event M Char, pusha) <- newEvent
    (eb :: Event M Int, pushb) <- newEvent
    bb <- synchronously $ hold 0 eb
    let ec = snapshotWith (,) ea bb
    outRef <- newIORef []
    unlisten <- synchronously $ listenIO ec $ \c -> modifyIORef outRef (++ [c])
    synchronously $ pusha 'A'
    synchronously $ pushb 50
    synchronously $ pusha 'B'
    synchronously $ pusha 'C' >> pushb 60
    synchronously $ pusha 'D'
    unlisten
    assertEqual "snapshot1" [('A',0),('B',50),('C',50),('D',60)] =<< readIORef outRef

count1 = TestCase $ do
    (ea :: Event M (), push) <- newEvent
    outRef <- newIORef []
    unlisten <- synchronously $ do
        eCount <- countE ea
        listenIO eCount $ \c -> modifyIORef outRef (++ [c])
    synchronously $ push ()
    synchronously $ push ()
    synchronously $ push ()
    unlisten
    assertEqual "count1" [1,2,3] =<< readIORef outRef

collect1 = TestCase $ do
    (ea :: Event M Int, push) <- newEvent
    outRef <- newIORef []
    unlisten <- synchronously $ do
        ba <- hold 100 ea
        sum <- collect (\a s -> (a+s, a+s)) 0 ba
        listenValueIO sum $ \sum -> modifyIORef outRef (++ [sum])
    synchronously $ push 5
    synchronously $ push 7
    synchronously $ push 1
    synchronously $ push 2
    synchronously $ push 3
    unlisten
    assertEqual "collect1" [100, 105, 112, 113, 115, 118] =<< readIORef outRef

collect2 = TestCase $ do
    (ea :: Event M Int, push) <- newEvent
    outRef <- newIORef []
    -- This behaviour is a little bit odd but difficult to fix in the
    -- implementation. However, it shouldn't be too much of a problem in
    -- practice. Here we are defining it.
    unlisten <- synchronously $ do
        ba <- hold 100 ea
        sum <- collect (\a s -> (a + s, a + s)) 0 ba
        push 5
        listenValueIO sum $ \sum -> modifyIORef outRef (++ [sum])
    synchronously $ push 7
    synchronously $ push 1
    unlisten
    assertEqual "collect2" [105, 112, 113] =<< readIORef outRef

collectE1 = TestCase $ do
    (ea :: Event M Int, push) <- newEvent
    outRef <- newIORef []
    unlisten <- synchronously $ do
        sum <- collectE (\a s -> (a+s, a+s)) 100 ea
        listenIO sum $ \sum -> modifyIORef outRef (++ [sum])
    synchronously $ push 5
    synchronously $ push 7
    synchronously $ push 1
    synchronously $ push 2
    synchronously $ push 3
    unlisten
    assertEqual "collectE1" [105, 112, 113, 115, 118] =<< readIORef outRef

collectE2 = TestCase $ do
    (ea :: Event M Int, push) <- newEvent
    outRef <- newIORef []
    -- This behaviour is a little bit odd but difficult to fix in the
    -- implementation. However, it shouldn't be too much of a problem in
    -- practice. Here we are defining it.
    unlisten <- synchronously $ do
        sum <- collectE (\a s -> (a + s, a + s)) 100 ea
        push 5
        listenIO sum $ \sum -> modifyIORef outRef (++ [sum])
    synchronously $ push 7
    synchronously $ push 1
    unlisten
    assertEqual "collectE2" [105, 112, 113] =<< readIORef outRef

switchE1 = TestCase $ do
    (ea :: Event M Char, pusha) <- newEvent
    (eb :: Event M Char, pushb) <- newEvent
    (esw :: Event M (Event M Char), pushsw) <- newEvent
    outRef <- newIORef []
    unlisten <- synchronously $ do
        sw <- hold ea esw
        let eo = switchE sw
        unlisten <- listenIO eo $ \o -> modifyIORef outRef (++ [o])
        pusha 'A'
        pushb 'a'
        return unlisten
    synchronously $ pusha 'B' >> pushb 'b'
    synchronously $ pushsw eb >> pusha 'C' >> pushb 'c'
    synchronously $ pusha 'D' >> pushb 'd'
    synchronously $ pusha 'E' >> pushb 'e' >> pushsw ea
    synchronously $ pusha 'F' >> pushb 'f'
    synchronously $ pusha 'G' >> pushb 'g' >> pushsw eb
    synchronously $ pusha 'H' >> pushb 'h' >> pushsw ea
    synchronously $ pusha 'I' >> pushb 'i' >> pushsw ea
    unlisten
    assertEqual "switchE1" "ABCdeFGhI" =<< readIORef outRef

switch1 = TestCase $ do
    (ea :: Event M Char, pusha) <- newEvent
    (eb :: Event M Char, pushb) <- newEvent
    (esw :: Event M (Behaviour M Char), pushsw) <- newEvent
    outRef <- newIORef []
    (ba, bb, unlisten) <- synchronously $ do
        ba <- hold 'A' ea
        bb <- hold 'a' eb
        bsw <- hold ba esw
        bo <- switch bsw
        unlisten <- listenValueIO bo $ \o -> modifyIORef outRef (++ [o])
        return (ba, bb, unlisten)
    synchronously $ pusha 'B' >> pushb 'b'
    synchronously $ pushsw bb >> pusha 'C' >> pushb 'c'
    synchronously $ pusha 'D' >> pushb 'd'
    synchronously $ pusha 'E' >> pushb 'e' >> pushsw ba
    synchronously $ pusha 'F' >> pushb 'f'
    synchronously $ pushsw bb
    synchronously $ pushsw ba
    synchronously $ pusha 'G' >> pushb 'g' >> pushsw bb
    synchronously $ pusha 'H' >> pushb 'h' >> pushsw ba
    synchronously $ pusha 'I' >> pushb 'i' >> pushsw ba
    unlisten
    assertEqual "switch1" "ABcdEFfFgHI" =<< readIORef outRef
    
once1 = TestCase $ do
    (ea :: Event M Char, pusha) <- newEvent
    outRef <- newIORef []
    unlisten <- synchronously $ do
        oea <- once ea
        listenIO oea $ \a -> modifyIORef outRef (++ [a])
    synchronously $ pusha 'A'
    synchronously $ pusha 'B'
    synchronously $ pusha 'C'
    unlisten
    assertEqual "switch1" "A" =<< readIORef outRef

once2 = TestCase $ do
    (ea :: Event M Char, pusha) <- newEvent
    outRef <- newIORef []
    unlisten <- synchronously $ do
        oea <- once ea
        pusha 'A'
        listenIO oea $ \a -> modifyIORef outRef (++ [a])
    synchronously $ pusha 'B'
    synchronously $ pusha 'C'
    unlisten
    assertEqual "switch1" "A" =<< readIORef outRef

crossE1 = TestCase $ do
    outRef <- newIORef []
    (ema :: Event M Char, push) <- newEvent
    (ena :: Event N Char) <- synchronously $ crossE ema
    unlisten <- synchronously $ listenIO ena $ \a -> modifyIORef outRef (++ [a])
    synchronously $ push 'A'
    synchronously $ push 'M'
    synchronously $ push 'T'
    -- Flush processing on partition N before unlistening
    synchronously (return () :: Reactive N ())
    unlisten
    assertEqual "crossE1" "AMT" =<< readIORef outRef

cross1 = TestCase $ do
    outRef <- newIORef []
    (ema :: Event M Char, push) <- newEvent
    bma <- synchronously $ hold 'A' ema
    synchronously $ push 'B'
    (bna :: Behaviour N Char) <- synchronously $ cross bma
    unlisten <- synchronously $ listenValueIO bna $ \a -> modifyIORef outRef (++ [a])
    synchronously $ push 'C'
    synchronously $ push 'D'
    synchronously $ push 'E'
    -- Flush processing on partition N before unlistening
    synchronously (return () :: Reactive N ())
    unlisten
    assertEqual "cross1" "BCDE" =<< readIORef outRef

cross2 = TestCase $ do
    outRef <- newIORef []
    (ema :: Event M Char, push) <- newEvent
    bma <- synchronously $ hold 'A' ema
    (bna :: Behaviour N Char) <- synchronously $ cross bma
    unlisten <- synchronously $ listenValueIO bna $ \a -> modifyIORef outRef (++ [a])
    synchronously $ push 'B'
    synchronously $ push 'C'
    synchronously $ push 'D'
    synchronously $ push 'E'
    -- Flush processing on partition N before unlistening
    synchronously (return () :: Reactive N ())
    unlisten
    assertEqual "cross1" "ABCDE" =<< readIORef outRef

data Page p = Page { unPage :: Reactive p (Char, Event p (Page p)) }

cycle1 = TestCase $ do
    outRef <- newIORef []
    (ep :: Event M (Page M), push) <- newEvent
    bo <- synchronously $ do
        let initPair = ('a', ep)
        rec
            bPair <- hold initPair ePage
            let ePage = execute $ unPage <$> switchE (snd <$> bPair)
        return (fst <$> bPair)
    unlisten <- synchronously $ listenValueIO bo $ \o -> modifyIORef outRef (++ [o])
    synchronously $ push (Page $ return ('b', ep))
    synchronously $ push (Page $ return ('c', ep))
    unlisten
    assertEqual "cycle1" "abc" =<< readIORef outRef

mergeWith1 = TestCase $ do
    outRef <- newIORef []
    (ea :: Event M Int, pushA) <- newEvent
    (eb :: Event M Int, pushB) <- newEvent
    unlisten <- synchronously $ do
        pushA 5
        listenIO (mergeWith (+) ea eb) $ \o -> modifyIORef outRef (++ [o])
    synchronously $ pushA 2
    synchronously $ pushB 3
    synchronously $ pushA 10 >> pushB 4
    synchronously $ pushB 7 >> pushA 1
    unlisten
    assertEqual "mergeWith1" [5,2,3,14,8] =<< readIORef outRef

mergeWith2 = TestCase $ do
    outRef <- newIORef []
    (ea :: Event M Int, pushA) <- newEvent
    (eb :: Event M Int, pushB) <- newEvent
    unlisten <- synchronously $ do
        pushA 5
        unlisten <- listenIO (mergeWith (+) ea eb) $ \o -> modifyIORef outRef (++ [o])
        pushB 99
        return unlisten
    unlisten
    assertEqual "mergeWith2" [104] =<< readIORef outRef

mergeWith3 = TestCase $ do
    outRef <- newIORef []
    (ea :: Event M Int, pushA) <- newEvent
    (eb :: Event M Int, pushB) <- newEvent
    unlisten <- synchronously $ do
        listenIO (mergeWith (+) ea eb) $ \o -> modifyIORef outRef (++ [o])
    synchronously $ pushA 2
    synchronously $ pushB 3 >> pushB 1 >> pushA 10
    synchronously $ pushB 9 >> pushB 11 >> pushB 12
    synchronously $ pushA 32 >> pushA 11 >> pushA 12
    unlisten
    assertEqual "mergeWith3" [2,14,32,55] =<< readIORef outRef

calm1 = TestCase $ do
    outRef <- newIORef []
    (ea :: Event M Int, pushA) <- newEvent
    (eb :: Event M Int, pushB) <- newEvent
    unlisten <- synchronously $ do
        listenIO (calm (+) (merge ea eb)) $ \o -> modifyIORef outRef (++ [o])
    synchronously $ pushA 2
    synchronously $ pushA 5 >> pushB 6
    unlisten
    assertEqual "calm1" [2, 11] =<< readIORef outRef

tests = test [ event1, fmap1, merge1, filterJust1, filterE1, beh1, beh2, beh3, beh4, beh5, beh6,
    appl1, appl2, snapshot1, count1, collect1, collect2, collectE1, collectE2, switchE1,
    switch1, once1, once2, crossE1, cross1, cross2, cycle1, mergeWith1, mergeWith2, mergeWith3,
    calm1 ]

main = {-forever $-} runTestTT tests

