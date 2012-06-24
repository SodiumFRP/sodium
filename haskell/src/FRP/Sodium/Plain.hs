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
            -- | Internal: Extract the underlyingEvent event for this behavior.
            underlyingEvent :: Event a,
            -- | Obtain the current value of a behavior.
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
    once = once

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
                mTask <- lift $ popPriorityQueue queue2
                case mTask of
                    Just (Reactive task) -> do
                        task
                        loop
                    Nothing -> do
                        final <- gets asFinal
                        if not $ Seq.null final then do
                            let Reactive task = Seq.index final 0
                            modify $ \as -> as { asFinal = Seq.drop 1 final }
                            task
                            loop
                          else
                            return ()
    outVar <- newIORef undefined
    let lock = paLock partition
    putMVar lock ()
    q <- newPriorityQueue
    evalStateT loop $ ReactiveState {
            asQueue1 = Seq.singleton (task >>= ioReactive . writeIORef outVar),
            asQueue2 = q,
            asFinal = Seq.empty
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
--
-- To listen to a 'Behavior' use @listen (values b) handler@
listen        :: Event a -> (a -> IO ()) -> Reactive (IO ())
listen ev handle = listenTrans ev (ioReactive . handle)

-- | An event that never fires.
never         :: Event a
never = Event {
        getListenRaw = return $ Listen $ \_ _ _ -> return (return ()), 
        evCacheRef   = unsafePerformIO $ newIORef Nothing
    }

-- | Merge two streams of events of the same type.
--
-- In the case where two event occurrences are simultaneous (i.e. both
-- within the same transaction), both will be delivered in the same
-- transaction. If the event firings are ordered for some reason, then
-- their ordering is retained. In many common cases the ordering will
-- be undefined.
merge         :: Event a -> Event a -> Event a
merge ea eb = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        l1 <- getListen ea
        l2 <- getListen eb                                
        (l, push, nodeRef) <- ioReactive newEventImpl
        unlistener1 <- unlistenize $ runListen l1 (Just nodeRef) False push
        unlistener2 <- unlistenize $ runListen l2 (Just nodeRef) False push
        (addCleanup unlistener1 <=< addCleanup unlistener2) l

-- | Unwrap Just values, and discard event occurrences with Nothing values.
filterJust    :: Event (Maybe a) -> Event a
filterJust ema = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l', push, nodeRef) <- ioReactive newEventImpl
        l <- getListen ema
        unlistener <- unlistenize $ runListen l (Just nodeRef) False $ \ma -> case ma of
            Just a -> push a
            Nothing -> return ()
        addCleanup unlistener l'

-- | Create a behavior with the specified initial value, that gets updated
-- by the values coming through the event. The \'current value\' of the behavior
-- is notionally the value as it was 'at the start of the transaction'.
-- That is, state updates caused by event firings get processed at the end of
-- the transaction.
hold          :: a -> Event a -> Reactive (Behavior a)
hold initA ea = do
    bsRef <- ioReactive $ newIORef (BehaviorState initA Nothing)
    unlistener <- unlistenize $ {-lastFiringOnly-} (linkedListen ea) Nothing False $ \a -> do
        bs <- ioReactive $ readIORef bsRef
        ioReactive $ writeIORef bsRef $ bs { bsUpdate = Just a }
        when (isNothing (bsUpdate bs)) $ scheduleLast $ ioReactive $ do
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

-- | An event that gives the updates for the behavior. It doesn't do any equality
-- comparison as the name might imply.
changes       :: Behavior a -> Event a
changes = underlyingEvent

-- | An event that is guaranteed to fires once when you listen to it, giving
-- the current value of the behavior, and thereafter behaves like 'changes',
-- firing for each update to the behavior's value.
values        :: Behavior a -> Event a
values = eventify . listenValueRaw

-- | Sample the behavior at the time of the event firing. Note that the 'current value'
-- of the behavior that's sampled is the value as at the start of the transaction
-- before any state changes of the current transaction are applied through 'hold's.
snapshotWith  :: (a -> b -> c) -> Event a -> Behavior b -> Event c
snapshotWith f ea bb = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l, push, nodeRef) <- ioReactive newEventImpl
        unlistener <- unlistenize $ linkedListen ea (Just nodeRef) False $ \a -> do
            b <- sample bb
            push (f a b)
        addCleanup unlistener l

-- | Unwrap an event inside a behavior to give a time-varying event implementation.
switchE       :: Behavior (Event a) -> Event a
switchE bea = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l, push, nodeRef) <- ioReactive newEventImpl
        unlisten2Ref <- ioReactive $ newIORef Nothing
        let doUnlisten2 = do
                mUnlisten2 <- readIORef unlisten2Ref
                fromMaybe (return ()) mUnlisten2
        unlistener1 <- unlistenize $ do
            initEa <- sample bea
            (ioReactive . writeIORef unlisten2Ref) =<< (Just <$> linkedListen initEa (Just nodeRef) False push)
            unlisten1 <- linkedListen (changes bea) (Just nodeRef) False $ \ea -> scheduleLast $ do
                ioReactive doUnlisten2
                (ioReactive . writeIORef unlisten2Ref) =<< (Just <$> linkedListen ea (Just nodeRef) True push)
            return $ unlisten1 >> doUnlisten2
        addCleanup unlistener1 l

-- | Unwrap a behavior inside another behavior to give a time-varying behavior implementation.
switch        :: Behavior (Behavior a) -> Reactive (Behavior a)
switch bba = do
    ba <- sample bba
    za <- sample ba
    (ev, push, nodeRef) <- ioReactive newEventLinked
    unlisten2Ref <- ioReactive $ newIORef Nothing
    let doUnlisten2 = do
            mUnlisten2 <- readIORef unlisten2Ref
            fromMaybe (return ()) mUnlisten2
    unlisten1 <- listenValueRaw bba (Just nodeRef) False $ \ba -> do
        ioReactive doUnlisten2
        (ioReactive . writeIORef unlisten2Ref . Just) =<< listenValueRaw ba (Just nodeRef) False push
    hold za (finalizeEvent ev (unlisten1 >> doUnlisten2))

-- | Execute the specified 'Reactive' action inside an event.
execute       :: Event (Reactive a) -> Event a
execute ev = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l', push, nodeRef) <- ioReactive newEventImpl
        unlistener <- unlistenize $ do
            l <- getListen ev
            runListen l (Just nodeRef) False $ \action -> action >>= push
        addCleanup unlistener l'

-- | Obtain the current value of a behavior.
sample        :: Behavior a -> Reactive a
sample = behSample

-- | If there's more than one firing in a single transaction, combine them into
-- one using the specified combining function.
--
-- If the event firings are ordered, then the first will appear at the left
-- input of the combining function. In most common cases it's best not to
-- make any assumptions about the ordering, and the combining function would
-- ideally be commutative.
coalesce      :: (a -> a -> a) -> Event a -> Event a
coalesce combine e = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        l1 <- getListen e
        (l, push, nodeRef) <- ioReactive newEventImpl
        outRef <- ioReactive $ newIORef Nothing  
        unlistener <- unlistenize $ runListen l1 (Just nodeRef) False $ \a -> do
            first <- isNothing <$> ioReactive (readIORef outRef)
            ioReactive $ modifyIORef outRef $ \ma -> Just $ case ma of
                Just a0 -> a0 `combine` a
                Nothing -> a
            when first $ schedulePrioritized (Just nodeRef) $ do
                Just out <- ioReactive $ readIORef outRef
                ioReactive $ writeIORef outRef Nothing
                push out
        addCleanup unlistener l

-- | Throw away all event occurrences except for the first one.
once :: Event a -> Event a
once e = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        l1 <- getListen e
        (l, push, nodeRef) <- ioReactive newEventImpl
        aliveRef <- ioReactive $ newIORef True
        unlistener <- unlistenize $ do
            rec
                unlisten <- runListen l1 (Just nodeRef) False $ \a -> do
                    alive <- ioReactive $ readIORef aliveRef
                    when alive $ do
                        ioReactive $ writeIORef aliveRef False
                        scheduleLast $ ioReactive unlisten
                        push a
            return unlisten
        addCleanup unlistener l

-- | Create a new 'Behavior' along with an action to push changes into it.
-- American spelling.
newBehavior :: a  -- ^ Initial behavior value
            -> Reactive (Behavior a, a -> Reactive ())
newBehavior = R.newBehavior

-- | Create a new 'Behavior' along with an action to push changes into it.
-- British spelling.
newBehaviour :: a  -- ^ Initial behavior value
            -> Reactive (Behavior a, a -> Reactive ())
newBehaviour = R.newBehaviour

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

-- | Variant of 'snapshotWith' that throws away the event's value and captures the behavior's.
snapshot :: Event a -> Behavior b -> Event b
snapshot = R.snapshot

-- | Let event occurrences through only when the behavior's value is True.
-- Note that the behavior's value is as it was at the start of the transaction,
-- that is, no state changes from the current transaction are taken into account.
gate :: Event a -> Behavior Bool -> Event a
gate = R.gate

-- | Transform an event with a generalized state loop (a mealy machine). The function
-- is passed the input and the old state and returns the new state and output value.
collectE :: (a -> s -> (b, s)) -> s -> Event a -> Reactive (Event b)
collectE = R.collectE

-- | Transform a behavior with a generalized state loop (a mealy machine). The function
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

-- | Count event occurrences, giving a behavior that starts with 0 before the first occurrence.
count :: Event a -> Reactive (Behavior Int)
count = R.count

class PriorityQueueable k where
    priorityOf       :: k -> IO Int64

newtype Sequence = Sequence Int64 deriving (Eq, Ord, Enum)

data PriorityQueue k v = PriorityQueue {
        pqNextSeq    :: IORef Sequence,
        pqDirty      :: IORef Bool,
        pqQueue      :: IORef (Map (Int64, Sequence) v),
        pqData       :: IORef (Map Sequence (k, v))
    }

newPriorityQueue :: IO (PriorityQueue k v)
newPriorityQueue =
    PriorityQueue <$> newIORef (Sequence 0) <*> newIORef False
                  <*> newIORef M.empty <*> newIORef M.empty

pushPriorityQueue :: PriorityQueueable k => PriorityQueue k v -> k -> v -> IO ()
pushPriorityQueue pq k v = do
    prio <- priorityOf k
    seq <- readIORef (pqNextSeq pq)
    modifyIORef (pqNextSeq pq) succ
    modifyIORef (pqQueue pq) (M.insert (prio, seq) v)
    modifyIORef (pqData pq)  (M.insert seq (k, v))

dirtyPriorityQueue :: PriorityQueue k v -> IO ()
dirtyPriorityQueue pq = writeIORef (pqDirty pq) True

popPriorityQueue :: PriorityQueueable k => PriorityQueue k v -> IO (Maybe v)
popPriorityQueue pq = do
    maybeRegen
    q <- readIORef (pqQueue pq)
    if M.null q
        then return Nothing
        else do
            let (pseq@(prio, seq), v) = M.findMin q
            modifyIORef (pqQueue pq) (M.delete pseq)
            modifyIORef (pqData pq)  (M.delete seq)
            return $ Just v
  where
    maybeRegen = do
        dirty <- readIORef (pqDirty pq)
        when dirty $ do
            writeIORef (pqDirty pq) False
            dat <- readIORef (pqData pq)
            writeIORef (pqQueue pq) M.empty
            forM_ (M.assocs dat) $ \(seq,(k,v)) -> do
                prio <- priorityOf k
                modifyIORef (pqQueue pq) (M.insert (prio, seq) v)

type ID = Int64

instance PriorityQueueable (Maybe (IORef Node)) where
    priorityOf (Just nodeRef) = noRank <$> readIORef nodeRef
    priorityOf Nothing        = return maxBound

data ReactiveState = ReactiveState {
        asQueue1     :: Seq (Reactive ()),
        asQueue2     :: PriorityQueue (Maybe (IORef Node)) (Reactive ()),
        asFinal      :: Seq (Reactive ())
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
scheduleEarly :: Reactive () -> Reactive ()
scheduleEarly task = Reactive $ modify $ \as -> as { asQueue1 = asQueue1 as |> task }

scheduleLast :: Reactive () -> Reactive ()
scheduleLast task = Reactive $ modify $ \as -> as { asFinal = asFinal as |> task }

data Listen a = Listen { runListen_ :: Maybe (IORef Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ()) }

runListen :: Listen a -> Maybe (IORef Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ())
{-# NOINLINE runListen #-}
runListen l mv suppressEarlierFirings handle = do
    o <- runListen_ l mv suppressEarlierFirings handle
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
linkedListen :: Event a -> Maybe (IORef Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ())
linkedListen ev mMvTarget suppressEarlierFirings handle = do
    l <- getListen ev
    runListen l mMvTarget suppressEarlierFirings handle

-- | Variant of 'listen' that allows you to initiate more activity in the current
-- transaction. Useful for implementing new primitives.
listenTrans :: Event a -> (a -> Reactive ()) -> Reactive (IO ())
listenTrans ev handle = linkedListen ev Nothing False handle

data Observer p a = Observer {
        obNextID    :: ID,
        obListeners :: Map ID (a -> Reactive ()),
        obFirings   :: [a]
    }

data Node = Node {
        noID        :: NodeID,
        noRank      :: Int64,
        noListeners :: Map ID (IORef Node)
    }

newNode :: IO (IORef Node)
newNode = do
    nodeID <- readIORef (paNextNodeID partition)
    modifyIORef (paNextNodeID partition) succ
    newIORef (Node nodeID 0 M.empty)

wrap :: (Maybe (IORef Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ())) -> IO (Listen a)
{-# NOINLINE wrap #-}
wrap l = return (Listen l)

touch :: Listen a -> IO ()
{-# NOINLINE touch #-}
touch l = evaluate l >> return ()

linkNode :: IORef Node -> ID -> IORef Node -> IO Bool
linkNode nodeRef iD mvTarget = do
    no <- readIORef nodeRef
    modified <- ensureBiggerThan S.empty mvTarget (noRank no)
    modifyIORef nodeRef $ \no ->
        no { noListeners = M.insert iD mvTarget (noListeners no) }
    return modified

ensureBiggerThan :: Set NodeID -> IORef Node -> Int64 -> IO Bool
ensureBiggerThan visited nodeRef limit = do
    no <- readIORef nodeRef
    if noRank no > limit || noID no `S.member` visited then
            return False
        else do
            let newSerial = succ limit
            --putStrLn $ show (noRank no) ++ " -> " ++ show newSerial
            modifyIORef nodeRef $ \no -> no { noRank = newSerial }
            forM_ (M.elems . noListeners $ no) $ \mvTarget -> do
                ensureBiggerThan (S.insert (noID no) visited) mvTarget newSerial
            return True

unlinkNode :: IORef Node -> ID -> IO ()
unlinkNode nodeRef iD = do
    modifyIORef nodeRef $ \no ->
        no { noListeners = M.delete iD (noListeners no) }

-- | Returns a 'Listen' for registering listeners, and a push action for pushing
-- a value into the event.
newEventImpl :: forall p a . IO (Listen a, a -> Reactive (), IORef Node)
newEventImpl = do
    nodeRef <- newNode
    mvObs <- newMVar (Observer 0 M.empty [])
    cacheRef <- newIORef Nothing
    rec
        let l mMvTarget suppressEarlierFirings handle = do
                (firings, unlisten, iD) <- ioReactive $ modifyMVar mvObs $ \ob -> return $
                    let iD = obNextID ob
                        handle' a = handle a >> ioReactive (touch listen)
                        ob' = ob { obNextID    = succ iD,
                                   obListeners = M.insert iD handle' (obListeners ob) }
                        unlisten = do
                            modifyMVar_ mvObs $ \ob -> return $ ob {
                                    obListeners = M.delete iD (obListeners ob)
                                }
                            unlinkNode nodeRef iD
                            return ()
                    in (ob', (reverse . obFirings $ ob, unlisten, iD))
                modified <- case mMvTarget of
                    Just mvTarget -> ioReactive $ linkNode nodeRef iD mvTarget
                    Nothing       -> return False
                -- If any of our ranks are changed, dirty the priority queue so
                -- all the priorities will be re-evaluated
                when modified $ dirtyPrioritized
                unless suppressEarlierFirings $ mapM_ handle firings
                return unlisten
        listen <- wrap l  -- defeat optimizer on ghc-7.0.4
    let push a = do
            ob <- ioReactive $ modifyMVar mvObs $ \ob -> return $
                (ob { obFirings = a : obFirings ob }, ob)
            -- If this is the first firing...
            when (null (obFirings ob)) $ scheduleLast $ ioReactive $ do
                modifyMVar_ mvObs $ \ob -> return $ ob { obFirings = [] }
            ioReactive $ evaluate a
            mapM_ ($ a) (M.elems . obListeners $ ob)
    return (listen, push, nodeRef)

-- | Returns an event, and a push action for pushing a value into the event.
newEventLinked :: IO (Event a, a -> Reactive (), IORef Node)
newEventLinked = do
    (listen, push, nodeRef) <- newEventImpl
    cacheRef <- newIORef Nothing
    let ev = Event {
                getListenRaw = return listen,
                evCacheRef = cacheRef
            }
    return (ev, push, nodeRef)

instance Functor (R.Event Plain) where
    f `fmap` Event getListen cacheRef = Event getListen' cacheRef
      where
        cacheRef = unsafePerformIO $ newIORef Nothing
        getListen' = do
            return $ Listen $ \mNodeRef suppressEarlierFirings handle -> do
                l <- getListen
                runListen l mNodeRef suppressEarlierFirings (handle . f)

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

-- | Listen to an input event/behavior and return an 'Unlistener' that can be
-- attached to an output event using 'addCleanup'.
unlistenize :: Reactive (IO ()) -> Reactive Unlistener
unlistenize doListen = do
    unlistener@(Unlistener ref) <- newUnlistener
    scheduleEarly $ do
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

-- | Listen to the value of this behavior with an initial callback giving
-- the current value. Can get multiple values per transaction, the last of
-- which is considered valid. You would normally want to use 'listenValue',
-- which removes the extra unwanted values.
listenValueRaw :: Behavior a -> Maybe (IORef Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ())
listenValueRaw ba = lastFiringOnly $ \mNodeRef suppressEarlierFirings handle -> do
    a <- sample ba
    handle a
    linkedListen (underlyingEvent ba) mNodeRef suppressEarlierFirings handle

-- | Queue the specified atomic to run at the end of the priority 2 queue
schedulePrioritized :: Maybe (IORef Node)
                    -> Reactive ()
                    -> Reactive ()
schedulePrioritized mNodeRef task = Reactive $ do
    q <- gets asQueue2
    lift $ pushPriorityQueue q mNodeRef task

dirtyPrioritized :: Reactive ()
dirtyPrioritized = Reactive $ do
    q <- gets asQueue2
    lift $ dirtyPriorityQueue q

-- Clean up the listener so it gives only one value per transaction, specifically
-- the last one.                
lastFiringOnly :: (Maybe (IORef Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ()))
     -> Maybe (IORef Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ())
lastFiringOnly listen mNodeRef suppressEarlierFirings handle = do
    aRef <- ioReactive $ newIORef Nothing
    listen mNodeRef suppressEarlierFirings $ \a -> do
        ma <- ioReactive $ readIORef aRef
        ioReactive $ writeIORef aRef (Just a)
        when (isNothing ma) $ schedulePrioritized mNodeRef $ do
            Just a <- ioReactive $ readIORef aRef
            ioReactive $ writeIORef aRef Nothing
            handle a

-- | Variant of 'listenValue' that allows you to initiate more activity in the current
-- transaction. Useful for implementing new primitives.
listenValueTrans :: Behavior a -> (a -> Reactive ()) -> Reactive (IO ())
listenValueTrans ba = listenValueRaw ba Nothing False

eventify :: (Maybe (IORef Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ())) -> Event a
eventify listen = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l, push, nodeRef) <- ioReactive newEventImpl
        unlistener <- unlistenize $ listen (Just nodeRef) False push
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
            (l, push, nodeRef) <- ioReactive newEventImpl
            unlistener1 <- unlistenize $ runListen l1 (Just nodeRef) False $ \f -> do
                ioReactive $ writeIORef fRef f
                a <- ioReactive $ readIORef aRef
                push (f a)
            unlistener2 <- unlistenize $ runListen l2 (Just nodeRef) False $ \a -> do
                f <- ioReactive $ readIORef fRef
                ioReactive $ writeIORef aRef a
                push (f a)
            (addCleanup unlistener1 <=< addCleanup unlistener2) l
        s = ($) <$> s1 <*> s2

{-
-- | Cross the specified event over to a different partition.
crossE :: (Typeable p, Typeable q) => Event a -> Reactive (Event q a)
crossE epa = do
    (ev, push) <- ioReactive newEvent
    unlisten <- listen epa $ async . push
    return $ finalizeEvent ev unlisten

-- | Cross the specified behavior over to a different partition.
cross :: (Typeable p, Typeable q) => Behavior a -> Reactive (Behavior q a)
cross bpa = do
    a <- sample bpa
    ea <- crossE (underlyingEvent bpa)
    ioReactive $ sync $ 3 a ea
-}

