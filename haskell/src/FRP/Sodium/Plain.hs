{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, DoRec, GADTs,
    TypeFamilies, EmptyDataDecls, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module FRP.Sodium.Plain where

import qualified FRP.Sodium.Context as R

-- Note: the 'full-laziness' optimization messes up finalizers, so we're
-- disabling it. It'd be nice to find a really robust solution to this.
-- -fno-cse just in case, since we're using unsafePerformIO.

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Int
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import GHC.Exts
import System.Mem.Weak
import System.IO.Unsafe

-- | Phantom type for use with 'R.Context' type class.
data Plain

partition :: Partition
{-# NOINLINE partition #-}
partition = unsafePerformIO createPartition
  where
    createPartition :: IO Partition
    createPartition = do
        lock <- newEmptyMVar
        nextNodeIDRef <- newIORef (NodeID 0)
        return $ Partition {
                paLock       = lock,
                paNextNodeID = nextNodeIDRef
            }

-- | A monad for transactional reactive operations. Execute it from 'IO' using 'sync'.
type Reactive a = R.Reactive Plain a

-- | A stream of events. The individual firings of events are called \'event occurrences\'.
type Event a = R.Event Plain a

-- | A time-varying value, American spelling.
type Behavior a = R.Behavior Plain a

-- | A time-varying value, British spelling.
type Behaviour a = R.Behavior Plain a

instance R.Context Plain where

    data Reactive Plain a = Reactive (StateT ReactiveState IO a)

    data Event Plain a = Event {  -- Must be data not newtype, because we need to attach finalizers to it
            -- | Listen for event occurrences on this event, to be handled by the specified
            -- handler. The returned action is used to unregister the listener.
            getListenRaw :: Reactive (Listen a),
            evCacheRef   :: IORef (Maybe (Listen a))
        }

    data Behavior Plain a = Behavior {
            -- | Internal: Extract the underlyingEvent event for this behaviour.
            underlyingEvent :: Event a,
            -- | Obtain the current value of a behaviour.
            behSample       :: Reactive a
        }
    sync = sync
    ioReactive = ioReactive
    newEvent = newEvent
    listen = listen
    never = never
    merge = merge
    filterJust = filterJust
    hold = hold
    changes = changes
    values = values
    snapshotWith = snapshotWith
    switchE = switchE
    switch = switch
    execute = execute
    sample = sample
    coalesce = coalesce

-- | Execute the specified 'Reactive' within a new transaction, blocking the caller
-- until all resulting processing is complete and all callbacks have been called.
-- This operation is thread-safe, so it may be called from any thread.
--
-- State changes to 'hold' values occur after processing of the transaction is complete.
sync :: Reactive a -> IO a
sync task = do
    let loop :: StateT ReactiveState IO () = do
            queue1 <- gets asQueue1
            if not $ Seq.null queue1 then do
                let Reactive task = Seq.index queue1 0
                modify $ \as -> as { asQueue1 = Seq.drop 1 queue1 }
                task
                loop
              else do
                queue2 <- gets asQueue2
                if not $ M.null queue2 then do
                    let (k, Reactive task) = M.findMin queue2
                    modify $ \as -> as { asQueue2 = M.delete k queue2 }
                    task
                    loop
                  else do
                    final <- gets asFinal
                    liftIO final
                    return ()
    outVar <- newIORef undefined
    let lock = paLock partition
    putMVar lock ()
    evalStateT loop $ ReactiveState {
            asQueue1 = Seq.singleton (task >>= ioReactive . writeIORef outVar),
            asQueue2 = M.empty,
            asFinal = return ()
        }
    takeMVar lock
    readIORef outVar

-- | Returns an event, and a push action for pushing a value into the event.
newEvent      :: Reactive (Event a, a -> Reactive ())  
newEvent = do
    (ev, push, _) <- ioReactive newEventLinked
    return (ev, push)

-- | Listen for firings of this event. The returned @IO ()@ is an IO action
-- that unregisters the listener. This is the observer pattern.
listen        :: Event a -> (a -> IO ()) -> Reactive (IO ())
listen ev handle = listenTrans ev (ioReactive . handle)

-- | An event that never fires.
never         :: Event a
never = Event {
        getListenRaw = return $ Listen $ \_ _ -> return (return ()), 
        evCacheRef   = unsafePerformIO $ newIORef Nothing
    }

-- | Merge two streams of events of the same type.
--
-- In the case where two event occurrences are simultaneous (i.e. both
-- within the same transaction), both will be delivered in the same
-- transaction.
--
-- The order is not defined, because simultaneous events should be considered
-- to be order-agnostic.
merge         :: Event a -> Event a -> Event a
merge ea eb = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        l1 <- getListen ea
        l2 <- getListen eb                                
        (l, push, mvNode) <- ioReactive newEventImpl
        unlistener1 <- unlistenize $ runListen l1 (Just mvNode) push
        unlistener2 <- unlistenize $ runListen l2 (Just mvNode) push
        (addCleanup unlistener1 <=< addCleanup unlistener2) l

-- | Unwrap Just values, and discard event occurrences with Nothing values.
filterJust    :: Event (Maybe a) -> Event a
filterJust ema = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l', push, mvNode) <- ioReactive newEventImpl
        l <- getListen ema
        unlistener <- unlistenize $ runListen l (Just mvNode) $ \ma -> case ma of
            Just a -> push a
            Nothing -> return ()
        addCleanup unlistener l'

-- | Create a behaviour with the specified initial value, that gets updated
-- by the values coming through the event. The \'current value\' of the behaviour
-- is notionally the value as it was 'at the start of the transaction'.
-- That is, state updates caused by event firings get processed at the end of
-- the transaction.
hold          :: a -> Event a -> Reactive (Behavior a)
hold initA ea = do
    bsRef <- ioReactive $ newIORef (BehaviorState initA Nothing)
    unlistener <- unlistenize $ listenTrans ea $ \a -> do
        bs <- ioReactive $ readIORef bsRef
        ioReactive $ writeIORef bsRef $ bs { bsUpdate = Just a }
        when (isNothing (bsUpdate bs)) $ onFinal $ do
            bs <- readIORef bsRef
            let newCurrent = fromJust (bsUpdate bs)
                bs' = newCurrent `seq` BehaviorState newCurrent Nothing
            evaluate bs'
            writeIORef bsRef bs'
    let gl = do
            l <- getListen ea
            addCleanup unlistener l
        beh = Behavior {
                underlyingEvent = Event gl (evCacheRef ea),
                behSample = ioReactive $ bsCurrent <$> readIORef bsRef
            }
    return beh

-- | An event that gives the updates for the behaviour. It doesn't do any equality
-- comparison as the name might imply.
changes       :: Behavior a -> Event a
changes = underlyingEvent

-- | An event that is guaranteed to fires once when you listen to it, giving
-- the current value of the behaviour, and thereafter behaves like 'changes',
-- firing for each update to the behaviour's value.
values        :: Behavior a -> Event a
values = eventify . linkedListenValue

-- | Sample the behaviour at the time of the event firing. Note that the 'current value'
-- of the behaviour that's sampled is the value as at the start of the transaction
-- before any state changes of the current transaction are applied through 'hold's.
snapshotWith  :: (a -> b -> c) -> Event a -> Behavior b -> Event c
snapshotWith f ea bb = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l, push, mvNode) <- ioReactive newEventImpl
        unlistener <- unlistenize $ linkedListen ea (Just mvNode) $ \a -> do
            b <- sample bb
            push (f a b)
        addCleanup unlistener l

-- | Unwrap an event inside a behaviour to give a time-varying event implementation.
switchE       :: Behavior (Event a) -> Event a
switchE bea = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    unlistensRef = unsafePerformIO $ newIORef M.empty
    gl = do
        -- assign ID numbers to the incoming events
        beaId <- R.collect (\ea nxtID -> ((ea, nxtID), succ nxtID)) (0 :: ID) bea
        (l, push, mvNode) <- ioReactive newEventImpl
        unlistener1 <- unlistenize $ linkedListenValue beaId (Just mvNode) $ \(ea, iD) -> do
            let filtered = filterJust $ snapshotWith (\a activeID ->
                        if activeID == iD
                            then Just a
                            else Nothing
                    ) ea (snd <$> beaId)
            unlisten2 <- listenTrans filtered $ \a -> do
                push a
                ioReactive $ unlistenLessThan unlistensRef iD
            ioReactive $ modifyIORef unlistensRef (M.insert iD unlisten2)
        addCleanup unlistener1 l

-- | Unwrap a behaviour inside another behaviour to give a time-varying behaviour implementation.
switch        :: Behavior (Behavior a) -> Reactive (Behavior a)
switch bba = do
    ba <- sample bba
    za <- sample ba
    (ev, push, mvNode) <- ioReactive newEventLinked
    activeIDRef <- ioReactive $ newIORef (0 :: ID)
    unlistensRef <- ioReactive $ newIORef M.empty
    unlisten1 <- listenValueRaw bba (Just mvNode) $ \ba -> do
        iD <- ioReactive $ do
            modifyIORef activeIDRef succ
            readIORef activeIDRef
        unlisten2 <- listenValueRaw ba (Just mvNode) $ \a -> do
            activeID <- ioReactive $ readIORef activeIDRef
            when (activeID == iD) $ do
                push a
                ioReactive $ unlistenLessThan unlistensRef iD
        ioReactive $ modifyIORef unlistensRef (M.insert iD unlisten2)
    hold za (finalizeEvent ev unlisten1)

-- | Execute the specified 'Reactive' action inside an event.
execute       :: Event (Reactive a) -> Event a
execute ev = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l', push, mvNode) <- ioReactive newEventImpl
        unlistener <- unlistenize $ do
            l <- getListen ev
            runListen l (Just mvNode) $ \action -> action >>= push
        addCleanup unlistener l'

-- | Obtain the current value of a behaviour.
sample        :: Behavior a -> Reactive a
sample = behSample

-- | If there's more than one firing in a single transaction, combine them into
-- one using the specified combining function.
coalesce      :: (a -> a -> a) -> Event a -> Event a
coalesce combine e = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        l1 <- getListen e
        (l, push, mvNode) <- ioReactive newEventImpl
        outRef <- ioReactive $ newIORef Nothing  
        unlistener <- unlistenize $ runListen l1 (Just mvNode) $ \a -> do
            first <- isNothing <$> ioReactive (readIORef outRef)
            ioReactive $ modifyIORef outRef $ \ma -> Just $ case ma of
                Just a0 -> a0 `combine` a
                Nothing -> a
            when first $ scheduleLast (Just mvNode) $ do
                Just out <- ioReactive $ readIORef outRef
                ioReactive $ writeIORef outRef Nothing
                push out
        addCleanup unlistener l

newBehavior :: a  -- ^ Initial behaviour value
            -> Reactive (Behavior a, a -> Reactive ())
newBehavior = R.newBehavior

listenValue   :: Behavior a -> (a -> IO ()) -> Reactive (IO ())
listenValue = R.listenValue

-- | Merge two streams of events of the same type, combining simultaneous
-- event occurrences.
--
-- In the case where multiple event occurrences are simultaneous (i.e. all
-- within the same transaction), they are combined using the supplied
-- function. The output event is guaranteed not to have more than one
-- event occurrence per transaction.
--
-- The combine function should be commutative, because simultaneous events
-- should be considered to be order-agnostic.
mergeWith :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeWith = R.mergeWith

-- | Only keep event occurrences for which the predicate is true.
filterE :: (a -> Bool) -> Event a -> Event a
filterE = R.filterE

-- | Variant of 'snapshotWith' that throws away the event's value and captures the behaviour's.
snapshot :: Event a -> Behavior b -> Event b
snapshot = R.snapshot

-- | Let event occurrences through only when the behaviour's value is True.
-- Note that the behaviour's value is as it was at the start of the transaction,
-- that is, no state changes from the current transaction are taken into account.
gate :: Event a -> Behavior Bool -> Event a
gate = R.gate

-- | Transform an event with a generalized state loop (a mealy machine). The function
-- is passed the input and the old state and returns the new state and output value.
collectE :: (a -> s -> (b, s)) -> s -> Event a -> Reactive (Event b)
collectE = R.collectE

-- | Transform a behaviour with a generalized state loop (a mealy machine). The function
-- is passed the input and the old state and returns the new state and output value.
collect :: (a -> s -> (b, s)) -> s -> Behavior a -> Reactive (Behavior b)
collect = R.collect

-- | Accumulate on input event, outputting the new state each time.
accumE :: (a -> s -> s) -> s -> Event a -> Reactive (Event s) 
accumE = R.accumE

-- | Accumulate on input event, holding state.
accum :: (a -> s -> s) -> s -> Event a -> Reactive (Behavior s)
accum = R.accum

-- | Count event occurrences, starting with 1 for the first occurrence.
countE :: Event a -> Reactive (Event Int)
countE = R.countE

-- | Count event occurrences, giving a behaviour that starts with 0 before the first occurrence.
count :: Event a -> Reactive (Behavior Int)
count = R.count

-- | Throw away all event occurrences except for the first one.
once :: Event a -> Reactive (Event a)
once = R.once

type ID = Int64

data ReactiveState = ReactiveState {
        asQueue1 :: Seq (Reactive ()),
        asQueue2 :: Map Int64 (Reactive ()),
        asFinal  :: IO ()
    }

instance Functor (R.Reactive Plain) where
    fmap f rm = Reactive (fmap f (unReactive rm))

unReactive :: Reactive a -> StateT ReactiveState IO a
unReactive (Reactive m) = m

instance Applicative (R.Reactive Plain) where
    pure a = Reactive $ return a
    rf <*> rm = Reactive $ unReactive rf <*> unReactive rm

instance Monad (R.Reactive Plain) where
    return a = Reactive $ return a
    rma >>= kmb = Reactive $ do
        a <- unReactive rma
        unReactive (kmb a)

instance MonadFix (R.Reactive Plain) where
    mfix f = Reactive $ mfix $ \a -> unReactive (f a)

ioReactive :: IO a -> Reactive a
ioReactive io = Reactive $ liftIO io

newtype NodeID = NodeID Int deriving (Eq, Ord, Enum)

data Partition = Partition {
        paLock       :: MVar (),
        paNextNodeID :: IORef NodeID
    }

-- | Queue the specified atomic to run at the end of the priority 1 queue
schedulePrioritized :: Reactive () -> Reactive ()
schedulePrioritized task = Reactive $ modify $ \as -> as { asQueue1 = asQueue1 as |> task }

onFinal :: IO () -> Reactive ()
onFinal task = Reactive $ modify $ \as -> as { asFinal = asFinal as >> task }

data Listen a = Listen { runListen_ :: Maybe (MVar (Node)) -> (a -> Reactive ()) -> Reactive (IO ()) }

runListen :: Listen a -> Maybe (MVar (Node)) -> (a -> Reactive ()) -> Reactive (IO ())
{-# NOINLINE runListen #-}
runListen l mv handle = do
    o <- runListen_ l mv handle
    _ <- ioReactive $ evaluate l
    return o

-- | Unwrap an event's listener machinery.
getListen :: Event a -> Reactive (Listen a)
getListen (Event getLRaw cacheRef) = do
    mL <- ioReactive $ readIORef cacheRef
    case mL of
        Just l -> return l
        Nothing -> do
            l <- getLRaw
            ioReactive $ writeIORef cacheRef (Just l)
            return l

-- | Listen for firings of this event. The returned @IO ()@ is an IO action
-- that unregisters the listener. This is the observer pattern.
linkedListen :: Event a -> Maybe (MVar (Node)) -> (a -> Reactive ()) -> Reactive (IO ())
linkedListen ev mMvTarget handle = do
    l <- getListen ev
    runListen l mMvTarget handle

-- | Variant of 'listen' that allows you to initiate more activity in the current
-- transaction. Useful for implementing new primitives.
listenTrans :: Event a -> (a -> Reactive ()) -> Reactive (IO ())
listenTrans ev handle = linkedListen ev Nothing handle

data Observer p a = Observer {
        obNextID    :: ID,
        obListeners :: Map ID (a -> Reactive ()),
        obFirings   :: [a]
    }

data Node = Node {
        noID        :: NodeID,
        noRank      :: Int64,
        noListeners :: Map ID (MVar (Node))
    }

newNode :: IO (MVar (Node))
newNode = do
    nodeID <- readIORef (paNextNodeID partition)
    modifyIORef (paNextNodeID partition) succ
    newMVar (Node nodeID 0 M.empty)

wrap :: (Maybe (MVar (Node)) -> (a -> Reactive ()) -> Reactive (IO ())) -> IO (Listen a)
{-# NOINLINE wrap #-}
wrap l = return (Listen l)

touch :: Listen a -> IO ()
{-# NOINLINE touch #-}
touch l = evaluate l >> return ()

linkNode :: MVar (Node) -> ID -> MVar (Node) -> IO ()
linkNode mvNode iD mvTarget = do
    no <- readMVar mvNode
    ensureBiggerThan S.empty mvTarget (noRank no)
    modifyMVar_ mvNode $ \no -> return $
        no { noListeners = M.insert iD mvTarget (noListeners no) }

ensureBiggerThan :: Set NodeID -> MVar (Node) -> Int64 -> IO ()
ensureBiggerThan visited mvNode limit = do
    no <- readMVar mvNode
    if noRank no > limit || noID no `S.member` visited then
            return ()
        else do
            let newSerial = succ limit
            --putStrLn $ show (noRank no) ++ " -> " ++ show newSerial
            modifyMVar_ mvNode $ \no -> return $ no { noRank = newSerial }
            forM_ (M.elems . noListeners $ no) $ \mvTarget -> do
                ensureBiggerThan (S.insert (noID no) visited) mvTarget newSerial

unlinkNode :: MVar (Node) -> ID -> IO ()
unlinkNode mvNode iD = do
    modifyMVar_ mvNode $ \no -> return $
        no { noListeners = M.delete iD (noListeners no) }

-- | Returns a 'Listen' for registering listeners, and a push action for pushing
-- a value into the event.
newEventImpl :: forall p a . IO (Listen a, a -> Reactive (), MVar (Node))
newEventImpl = do
    mvNode <- newNode
    mvObs <- newMVar (Observer 0 M.empty [])
    cacheRef <- newIORef Nothing
    rec
        let l mMvTarget handle = do
                (firings, unlisten, iD) <- ioReactive $ modifyMVar mvObs $ \ob -> return $
                    let iD = obNextID ob
                        handle' a = handle a >> ioReactive (touch listen)
                        ob' = ob { obNextID    = succ iD,
                                   obListeners = M.insert iD handle' (obListeners ob) }
                        unlisten = do
                            modifyMVar_ mvObs $ \ob -> return $ ob {
                                    obListeners = M.delete iD (obListeners ob)
                                }
                            unlinkNode mvNode iD
                            return ()
                    in (ob', (reverse . obFirings $ ob, unlisten, iD))
                case mMvTarget of
                    Just mvTarget -> ioReactive $ linkNode mvNode iD mvTarget
                    Nothing       -> return ()
                mapM_ handle firings
                return unlisten
        listen <- wrap l  -- defeat optimizer on ghc-7.0.4
    let push a = do
            ob <- ioReactive $ modifyMVar mvObs $ \ob -> return $
                (ob { obFirings = a : obFirings ob }, ob)
            -- If this is the first firing...
            when (null (obFirings ob)) $ onFinal $ do
                modifyMVar_ mvObs $ \ob -> return $ ob { obFirings = [] }
            let seqa = seq a a
            mapM_ ($ seqa) (M.elems . obListeners $ ob)
    return (listen, push, mvNode)

-- | Returns an event, and a push action for pushing a value into the event.
newEventLinked :: IO (Event a, a -> Reactive (), MVar (Node))
newEventLinked = do
    (listen, push, mvNode) <- newEventImpl
    cacheRef <- newIORef Nothing
    let ev = Event {
                getListenRaw = return listen,
                evCacheRef = cacheRef
            }
    return (ev, push, mvNode)

instance Functor (R.Event Plain) where
    f `fmap` Event getListen cacheRef = Event getListen' cacheRef
      where
        cacheRef = unsafePerformIO $ newIORef Nothing
        getListen' = do
            return $ Listen $ \mMvNode handle -> do
                l <- getListen
                runListen l mMvNode (handle . f)

instance Functor (R.Behavior Plain) where
    f `fmap` Behavior underlyingEvent sample =
        Behavior (f `fmap` underlyingEvent) (f `fmap` sample)

constant :: a -> Behavior a
constant a = Behavior {
        underlyingEvent = never,
        behSample = return a
    }

data BehaviorState a = BehaviorState {
        bsCurrent :: a,
        bsUpdate  :: Maybe a
    }

-- | Add a finalizer to an event.
finalizeEvent :: Event a -> IO () -> Event a
{-# NOINLINE finalizeEvent #-}
finalizeEvent ea unlisten = Event gl (evCacheRef ea)
  where
    gl = do
        l <- getListen ea
        ioReactive $ finalizeListen l unlisten

-- | Add a finalizer to a listener.
finalizeListen :: Listen a -> IO () -> IO (Listen a)
{-# NOINLINE finalizeListen #-}
finalizeListen l unlisten = do
    addFinalizer l unlisten
    return l

newtype Unlistener = Unlistener (MVar (Maybe (IO ())))

-- | Listen to an input event/behaviour and return an 'Unlistener' that can be
-- attached to an output event using 'addCleanup'.
unlistenize :: Reactive (IO ()) -> Reactive Unlistener
unlistenize doListen = do
    unlistener@(Unlistener ref) <- newUnlistener
    schedulePrioritized $ do
        mOldUnlisten <- ioReactive $ takeMVar ref
        case mOldUnlisten of
            Just _ -> do
                unlisten <- doListen
                ioReactive $ putMVar ref (Just unlisten)
            Nothing -> ioReactive $ putMVar ref mOldUnlisten
    return unlistener
  where
    newUnlistener :: Reactive Unlistener
    newUnlistener = Unlistener <$> ioReactive (newMVar (Just $ return ()))

-- | Cause the things listened to with unlistenize to be unlistened when the
-- specified listener is not referenced any more.
addCleanup :: Unlistener -> Listen a -> Reactive (Listen a)
addCleanup (Unlistener ref) l = ioReactive $ finalizeListen l $ do
    mUnlisten <- takeMVar ref
    fromMaybe (return ()) mUnlisten
    putMVar ref Nothing

-- | Listen to the value of this behaviour with an initial callback giving
-- the current value. Can get multiple values per transaction, the last of
-- which is considered valid. You would normally want to use 'listenValue',
-- which removes the extra unwanted values.
listenValueRaw :: Behavior a -> Maybe (MVar (Node)) -> (a -> Reactive ()) -> Reactive (IO ())
listenValueRaw ba mMvNode handle = do
    a <- sample ba
    handle a
    linkedListen (underlyingEvent ba) mMvNode handle

-- | Queue the specified atomic to run at the end of the priority 2 queue
scheduleLast :: Maybe (MVar (Node))
                  -> Reactive ()
                  -> Reactive ()
scheduleLast mMvNode task = do
    mNode <- case mMvNode of
        Just mvNode -> Just <$> ioReactive (readMVar mvNode)
        Nothing -> pure Nothing
    let priority = maybe maxBound noRank mNode
    Reactive $ modify $ \as -> as {
            asQueue2 = M.alter (\mOldTask -> Just $ case mOldTask of
                    Just oldTask -> oldTask >> task
                    Nothing      -> task) priority (asQueue2 as)
        }

-- Clean up the listener so it gives only one value per transaction, specifically
-- the last one.                
tidy :: (Maybe (MVar (Node)) -> (a -> Reactive ()) -> Reactive (IO ()))
     -> Maybe (MVar (Node)) -> (a -> Reactive ()) -> Reactive (IO ())
tidy listen mMvNode handle = do
    aRef <- ioReactive $ newIORef Nothing
    listen mMvNode $ \a -> do
        ma <- ioReactive $ readIORef aRef
        ioReactive $ writeIORef aRef (Just a)
        when (isNothing ma) $ scheduleLast mMvNode $ do
            Just a <- ioReactive $ readIORef aRef
            ioReactive $ writeIORef aRef Nothing
            handle a

-- | Listen to the value of this behaviour with a guaranteed initial callback
-- giving the current value, followed by callbacks for any updates. 
linkedListenValue :: Behavior a -> Maybe (MVar (Node)) -> (a -> Reactive ()) -> Reactive (IO ())
linkedListenValue ba = tidy (listenValueRaw ba)

-- | Variant of 'listenValue' that allows you to initiate more activity in the current
-- transaction. Useful for implementing new primitives.
listenValueTrans :: Behavior a -> (a -> Reactive ()) -> Reactive (IO ())
listenValueTrans ba = linkedListenValue ba Nothing

eventify :: (Maybe (MVar (Node)) -> (a -> Reactive ()) -> Reactive (IO ())) -> Event a
eventify listen = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l, push, mvNode) <- ioReactive newEventImpl
        unlistener <- unlistenize $ listen (Just mvNode) push
        addCleanup unlistener l

instance Applicative (R.Behavior Plain) where
    pure = constant
    Behavior u1 s1 <*> Behavior u2 s2 = Behavior u s
      where
        cacheRef = unsafePerformIO $ newIORef Nothing
        u = Event gl cacheRef
        gl = do
            fRef <- ioReactive . newIORef =<< s1
            aRef <- ioReactive . newIORef =<< s2
            l1 <- getListen u1
            l2 <- getListen u2
            (l, push, mvNode) <- ioReactive newEventImpl
            unlistener1 <- unlistenize $ runListen l1 (Just mvNode) $ \f -> do
                ioReactive $ writeIORef fRef f
                a <- ioReactive $ readIORef aRef
                push (f a)
            unlistener2 <- unlistenize $ runListen l2 (Just mvNode) $ \a -> do
                f <- ioReactive $ readIORef fRef
                ioReactive $ writeIORef aRef a
                push (f a)
            (addCleanup unlistener1 <=< addCleanup unlistener2) l
        s = ($) <$> s1 <*> s2

splitLessThan :: Ord k => k -> Map k a -> (Map k a, Map k a)
splitLessThan k m =
    let (lt, mEq, gt) = M.splitLookup k m
    in  (lt, case mEq of
            Just eq -> M.insert k eq gt
            Nothing -> gt)

unlistenLessThan :: IORef (Map ID (IO ())) -> ID -> IO ()
unlistenLessThan unlistensRef iD = do
    uls <- readIORef unlistensRef
    let (toDelete, uls') = splitLessThan iD uls
    do
        writeIORef unlistensRef uls'
        {-when (M.size toDelete > 0) $
            putStrLn $ "deleting "++show (M.size toDelete) -}
        forM_ (M.elems toDelete) $ \unl -> unl

{-
-- | Cross the specified event over to a different partition.
crossE :: (Typeable p, Typeable q) => Event a -> Reactive (Event q a)
crossE epa = do
    (ev, push) <- ioReactive newEvent
    unlisten <- listen epa $ async . push
    return $ finalizeEvent ev unlisten

-- | Cross the specified behaviour over to a different partition.
cross :: (Typeable p, Typeable q) => Behavior a -> Reactive (Behavior q a)
cross bpa = do
    a <- sample bpa
    ea <- crossE (underlyingEvent bpa)
    ioReactive $ sync $ hold a ea
-}

