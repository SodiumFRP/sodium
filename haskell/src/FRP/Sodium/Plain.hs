{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, DoRec, GADTs,
    TypeFamilies, EmptyDataDecls, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module FRP.Sodium.Plain where

import qualified FRP.Sodium.Context as R

-- Note: the 'full-laziness' optimization messes up finalizers, so we're
-- disabling it. It'd be nice to find a really robust solution to this.
-- -fno-cse just in case, since we're using unsafePerformIO.

import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, newMVar, readMVar)
import qualified Control.Concurrent.MVar as MV
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

modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar mv f = MV.modifyMVar mv $ \a -> do
    (a', b') <- f a
    evaluate a'
    return (a', b')

modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ mv f = MV.modifyMVar_ mv $ \a -> do
    a' <- f a
    evaluate a'
    return a'

putMVar :: MVar a -> a -> IO ()
putMVar mv a = do
    evaluate a
    MV.putMVar mv a

-- Note: the second parameter is a dummy, make it depend
-- on the last function parameter of the caller to make
-- sure that the IORef is freshly created every time

{-# NOINLINE unsafeNewIORef #-}
unsafeNewIORef :: a -> b -> IORef a
unsafeNewIORef v dummy = unsafePerformIO (newIORef v)

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

-- Must be data not newtype, because we need to attach finalizers to it
data Sample a = Sample { unSample_ :: IO (IO a) }

unSample :: Sample a -> IO a
{-# NOINLINE unSample #-}
unSample sample = do
    sample' <- unSample_ sample
    touch sample  -- Ensure 'sample' stays alive while it's
                  -- being used.
    sample'

instance R.Context Plain where

    newtype Reactive Plain a = Reactive (StateT ReactiveState IO a)

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
            behSample       :: Sample a
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
-- To listen to a 'Behavior' use @listen (values b) handler@ or
-- @listen (changes b) handler@
--
-- NOTE: The callback is called with the transaction held, so you cannot
-- use 'sync' inside a listener. You can delegate to another thread and have
-- that start the new transaction. If you want to do more processing in
-- the same transction, then you can use 'FRP.Sodium.Internal.listenTrans'
-- but this is discouraged unless you really need to write a new primitive.
listen        :: Event a -> (a -> IO ()) -> Reactive (IO ())
listen ev handle = listenTrans ev (ioReactive . handle)

-- | An event that never fires.
never         :: Event a
never = Event {
        getListenRaw = return $ Listen $ \_ _ _ -> return (return ()), 
        evCacheRef   = unsafeNewIORef Nothing undefined
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
    cacheRef = unsafeNewIORef Nothing eb
    gl = do
        l1 <- getListen ea
        l2 <- getListen eb                                
        (l, push, nodeRef) <- ioReactive newEventImpl
        unlistener <- unlistenize $ do
            u1 <- runListen l1 (Just nodeRef) False push
            u2 <- runListen l2 (Just nodeRef) False push
            return (u1 >> u2)
        addCleanup_Listen unlistener l

-- | Unwrap Just values, and discard event occurrences with Nothing values.
filterJust    :: Event (Maybe a) -> Event a
filterJust ema = Event gl cacheRef
  where
    cacheRef = unsafeNewIORef Nothing ema
    gl = do
        (l', push, nodeRef) <- ioReactive newEventImpl
        l <- getListen ema
        unlistener <- unlistenize $ runListen l (Just nodeRef) False $ \ma -> case ma of
            Just a -> push a
            Nothing -> return ()
        addCleanup_Listen unlistener l'

-- | Create a behavior with the specified initial value, that gets updated
-- by the values coming through the event. The \'current value\' of the behavior
-- is notionally the value as it was 'at the start of the transaction'.
-- That is, state updates caused by event firings get processed at the end of
-- the transaction.
hold          :: a -> Event a -> Reactive (Behavior a)
hold initA ea = do
    bsRef <- ioReactive $ newIORef $ initA `seq` BehaviorState initA Nothing
    unlistener <- unlistenize $ linkedListen ea Nothing False $ \a -> do
        bs <- ioReactive $ readIORef bsRef
        ioReactive $ writeIORef bsRef $ a `seq` bs { bsUpdate = Just a }
        when (isNothing (bsUpdate bs)) $ scheduleLast $ ioReactive $ do
            bs <- readIORef bsRef
            let newCurrent = fromJust (bsUpdate bs)
            writeIORef bsRef $ newCurrent `seq` BehaviorState newCurrent Nothing
    sample <- ioReactive $ addCleanup_Sample unlistener (Sample $ return $ bsCurrent <$> readIORef bsRef)
    let beh = sample `seq` Behavior {
                underlyingEvent = ea,
                behSample = sample
            }
    return beh

-- | An event that gives the updates for the behavior. It doesn't do any equality
-- comparison as the name might imply.
changes       :: Behavior a -> Event a
changes = underlyingEvent

-- | An event that is guaranteed to fire once when you listen to it, giving
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
    cacheRef = unsafeNewIORef Nothing bb
    gl = do
        (l, push, nodeRef) <- ioReactive newEventImpl
        sample' <- ioReactive $ unSample_ $ behSample $ bb
        _ <- ioReactive $ touch sample'
        unlistener <- unlistenize $ do
            unlisten <- linkedListen ea (Just nodeRef) False $ \a -> do
                b <- ioReactive $ sample'
                push (f a b)
                return ()
            return (unlisten >> touch bb)
        addCleanup_Listen unlistener l

-- | Unwrap an event inside a behavior to give a time-varying event implementation.
switchE       :: Behavior (Event a) -> Event a
switchE bea = Event gl cacheRef
  where
    cacheRef = unsafeNewIORef Nothing bea
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
        addCleanup_Listen unlistener1 l

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
    cacheRef = unsafeNewIORef Nothing ev
    gl = do
        (l', push, nodeRef) <- ioReactive newEventImpl
        l <- getListen ev
        unlistener <- unlistenize $ runListen l (Just nodeRef) False $ \action -> action >>= push
        addCleanup_Listen unlistener l'

-- | Obtain the current value of a behavior.
sample        :: Behavior a -> Reactive a
sample b = ioReactive . unSample . behSample $ b

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
    cacheRef = unsafeNewIORef Nothing e
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
        addCleanup_Listen unlistener l

-- | Throw away all event occurrences except for the first one.
once :: Event a -> Event a
once e = Event gl cacheRef
  where
    cacheRef = unsafeNewIORef Nothing e
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
        addCleanup_Listen unlistener l

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

-- | Accumulate state changes given in the input event.
accum :: a -> Event (a -> a) -> Reactive (Behavior a)
accum = R.accum

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

instance PriorityQueueable (Maybe (MVar Node)) where
    priorityOf (Just nodeRef) = noRank <$> readMVar nodeRef
    priorityOf Nothing        = return maxBound

data ReactiveState = ReactiveState {
        asQueue1     :: Seq (Reactive ()),
        asQueue2     :: PriorityQueue (Maybe (MVar Node)) (Reactive ()),
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

data Listen a = Listen { runListen_ :: Maybe (MVar Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ()) }

runListen :: Listen a -> Maybe (MVar Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ())
{-# NOINLINE runListen #-}
runListen l mv suppressEarlierFirings handle = do
    unlisten <- runListen_ l mv suppressEarlierFirings handle
    -- ensure l doesn't get cleaned up while runListen_ is running
    _ <- ioReactive $ touch l
    return unlisten

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
linkedListen :: Event a -> Maybe (MVar Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ())
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
        noListeners :: Map ID (MVar Node)
    }

newNode :: IO (MVar Node)
newNode = do
    nodeID <- readIORef (paNextNodeID partition)
    modifyIORef (paNextNodeID partition) succ
    newMVar (Node nodeID 0 M.empty)

wrap :: (Maybe (MVar Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ())) -> IO (Listen a)
{-# NOINLINE wrap #-}
wrap l = return (Listen l)

touch :: a -> IO ()
{-# NOINLINE touch #-}
touch a = evaluate a >> return ()

linkNode :: MVar Node -> ID -> MVar Node -> IO Bool
linkNode nodeRef iD mvTarget = do
    no <- readMVar nodeRef
    modified <- ensureBiggerThan S.empty mvTarget (noRank no)
    modifyMVar_ nodeRef $ \no -> return $
        let listeners' = M.insert iD mvTarget (noListeners no)
        in  listeners' `seq` no { noListeners = listeners' }
    return modified

ensureBiggerThan :: Set NodeID -> MVar Node -> Int64 -> IO Bool
ensureBiggerThan visited nodeRef limit = do
    no <- takeMVar nodeRef
    if noRank no > limit || noID no `S.member` visited then do
            putMVar nodeRef no
            return False
        else do
            let newSerial = succ limit
            --putStrLn $ show (noRank no) ++ " -> " ++ show newSerial
            putMVar nodeRef $ newSerial `seq` no { noRank = newSerial }
            forM_ (M.elems . noListeners $ no) $ \mvTarget -> do
                ensureBiggerThan (S.insert (noID no) visited) mvTarget newSerial
            return True

unlinkNode :: MVar Node -> ID -> IO ()
unlinkNode nodeRef iD = do
    modifyMVar_ nodeRef $ \no -> do
        let listeners' = M.delete iD (noListeners no)
        return $ listeners' `seq` no { noListeners = listeners' }

-- | Returns a 'Listen' for registering listeners, and a push action for pushing
-- a value into the event.
newEventImpl :: forall p a . IO (Listen a, a -> Reactive (), MVar Node)
newEventImpl = do
    nodeRef <- newNode
    mvObs <- newMVar (Observer 0 M.empty [])
    cacheRef <- newIORef Nothing
    rec
        let l mMvTarget suppressEarlierFirings handle = do
                (firings, unlisten, iD) <- ioReactive $ modifyMVar mvObs $ \ob -> do
                    let iD = obNextID ob
                        nextID' = succ iD
                        listeners' = M.insert iD handle (obListeners ob)
                        ob' = nextID' `seq` listeners' `seq` 
                              ob { obNextID    = nextID',
                                   obListeners = listeners' }
                        unlisten = do
                            modifyMVar_ mvObs $ \ob -> do
                                let listeners' = M.delete iD (obListeners ob)
                                return $ listeners' `seq` ob { obListeners = listeners' }
                            unlinkNode nodeRef iD
                            touch listen
                            return ()
                    return (ob', (reverse . obFirings $ ob, unlisten, iD))
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
            ioReactive $ evaluate a
            ob <- ioReactive $ modifyMVar mvObs $ \ob -> return $
                (ob { obFirings = a : obFirings ob }, ob)
            -- If this is the first firing...
            when (null (obFirings ob)) $ scheduleLast $ ioReactive $ do
                modifyMVar_ mvObs $ \ob -> return $ ob { obFirings = [] }
            mapM_ ($ a) (M.elems . obListeners $ ob)
    return (listen, push, nodeRef)

-- | Returns an event, and a push action for pushing a value into the event.
newEventLinked :: IO (Event a, a -> Reactive (), MVar Node)
newEventLinked = do
    (listen, push, nodeRef) <- newEventImpl
    cacheRef <- newIORef Nothing
    let ev = Event {
                getListenRaw = return listen,
                evCacheRef = cacheRef
            }
    return (ev, push, nodeRef)

instance Functor (R.Event Plain) where
    f `fmap` e = Event getListen' cacheRef
      where
        cacheRef = unsafeNewIORef Nothing e
        getListen' = do
            return $ Listen $ \mNodeRef suppressEarlierFirings handle -> do
                l <- getListen e
                runListen l mNodeRef suppressEarlierFirings (handle . f)

instance Functor (R.Behavior Plain) where
    f `fmap` Behavior underlyingEvent sample =
        sample' `seq` Behavior (f `fmap` underlyingEvent) sample'
        where sample' = Sample $ return $ f `fmap` unSample sample

constant :: a -> Behavior a
constant a = Behavior {
        underlyingEvent = never,
        behSample = Sample $ return $ return a
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

-- | Add a finalizer to a Reactive.
finalizeSample :: Sample a -> IO () -> IO (Sample a)
{-# NOINLINE finalizeSample #-}
finalizeSample s unlisten = do
    addFinalizer s unlisten
    return s

newtype Unlistener = Unlistener (MVar (Maybe (IO ())))

-- | Listen to an input event/behavior and return an 'Unlistener' that can be
-- attached to an output event using 'addCleanup_Listen'.
unlistenize :: Reactive (IO ()) -> Reactive Unlistener
unlistenize doListen = do
    unlistener@(Unlistener ref) <- newUnlistener
    -- We schedule the actual listen rather than doing it now, so event values get
    -- evaluated lazily. Otherwise we get deadlocks when events are used in value loops.
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
addCleanup_Listen :: Unlistener -> Listen a -> Reactive (Listen a)
addCleanup_Listen (Unlistener ref) l = ioReactive $ finalizeListen l $ do
    mUnlisten <- takeMVar ref
    fromMaybe (return ()) mUnlisten
    putMVar ref Nothing

addCleanup_Sample :: Unlistener -> Sample a -> IO (Sample a)
addCleanup_Sample (Unlistener ref) r = finalizeSample r $ do
    mUnlisten <- takeMVar ref
    fromMaybe (return ()) mUnlisten
    putMVar ref Nothing

-- | Listen to the value of this behavior with an initial callback giving
-- the current value. Can get multiple values per transaction, the last of
-- which is considered valid. You would normally want to use 'listenValue',
-- which removes the extra unwanted values.
listenValueRaw :: Behavior a -> Maybe (MVar Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ())
listenValueRaw ba = lastFiringOnly $ \mNodeRef suppressEarlierFirings handle -> do
    a <- sample ba
    handle a
    linkedListen (underlyingEvent ba) mNodeRef suppressEarlierFirings handle

-- | Queue the specified atomic to run at the end of the priority 2 queue
schedulePrioritized :: Maybe (MVar Node)
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
lastFiringOnly :: (Maybe (MVar Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ()))
                -> Maybe (MVar Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ())
lastFiringOnly listen mNodeRef suppressEarlierFirings handle = do
    aRef <- ioReactive $ newIORef Nothing
    listen mNodeRef suppressEarlierFirings $ \a -> do
        ma <- ioReactive $ readIORef aRef
        ioReactive $ writeIORef aRef (Just a)
        when (isNothing ma) $ schedulePrioritized mNodeRef $ do
            Just a <- ioReactive $ readIORef aRef
            ioReactive $ writeIORef aRef Nothing
            handle a

eventify :: (Maybe (MVar Node) -> Bool -> (a -> Reactive ()) -> Reactive (IO ())) -> Event a
eventify listen = Event gl cacheRef
  where
    cacheRef = unsafeNewIORef Nothing listen
    gl = do
        (l, push, nodeRef) <- ioReactive newEventImpl
        unlistener <- unlistenize $ listen (Just nodeRef) False push
        addCleanup_Listen unlistener l

instance Applicative (R.Behavior Plain) where
    pure = constant
    Behavior u1 s1 <*> Behavior u2 s2 = Behavior u s
      where
        cacheRef = unsafeNewIORef Nothing s2
        u = Event gl cacheRef
        gl = do
            fRef <- ioReactive $ newIORef =<< unSample s1
            aRef <- ioReactive $ newIORef =<< unSample s2
            l1 <- getListen u1
            l2 <- getListen u2
            (l, push, nodeRef) <- ioReactive newEventImpl
            unlistener <- unlistenize $ do
                un1 <- runListen l1 (Just nodeRef) False $ \f -> do
                    ioReactive $ writeIORef fRef f
                    a <- ioReactive $ readIORef aRef
                    push (f a)
                un2 <- runListen l2 (Just nodeRef) False $ \a -> do
                    f <- ioReactive $ readIORef fRef
                    ioReactive $ writeIORef aRef a
                    push (f a)
                return (un1 >> un2)
            addCleanup_Listen unlistener l
        s = Sample $ return $ ($) <$> unSample s1 <*> unSample s2

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

