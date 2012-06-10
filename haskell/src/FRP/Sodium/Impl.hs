{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, DoRec, GADTs #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module FRP.Sodium.Impl where

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
import Data.Typeable
import GHC.Exts
import System.Mem.Weak
import System.IO.Unsafe
import Unsafe.Coerce

type ID = Int64

data ReactiveState p = ReactiveState {
        asQueue1 :: Seq (Reactive p ()),
        asQueue2 :: Map Int64 (Reactive p ()),
        asFinal  :: IO ()
    }

{-
newtype Reactive p a = Reactive (StateT (ReactiveState p) IO a)
    deriving (Functor, Applicative, Monad, MonadFix)
-}

data Reactive p a where
    Reactive :: StateT (ReactiveState p) IO a -> Reactive p a

instance Functor (Reactive p) where
    fmap f rm = Reactive (fmap f (unReactive rm))

unReactive :: Reactive p a -> StateT (ReactiveState p) IO a
unReactive (Reactive m) = m

instance Applicative (Reactive p) where
    pure a = Reactive $ return a
    rf <*> rm = Reactive $ unReactive rf <*> unReactive rm

instance Monad (Reactive p) where
    return a = Reactive $ return a
    rma >>= kmb = Reactive $ do
        a <- unReactive rma
        unReactive (kmb a)

instance MonadFix (Reactive p) where
    mfix f = Reactive $ mfix $ \a -> unReactive (f a)

ioReactive :: IO a -> Reactive p a
ioReactive io = Reactive $ liftIO io

newtype NodeID = NodeID Int deriving (Eq, Ord, Enum)

data Partition p = Partition {
        paRun        :: Reactive p () -> IO () -> IO (),
        paNextNodeID :: IORef NodeID
    }

-- | Queue the specified atomic to run at the end of the priority 1 queue
schedulePriority1 :: Reactive p () -> Reactive p ()
schedulePriority1 task = Reactive $ modify $ \as -> as { asQueue1 = asQueue1 as |> task }

onFinal :: IO () -> Reactive p ()
onFinal task = Reactive $ modify $ \as -> as { asFinal = asFinal as >> task }

partitionRegistry :: MVar (Map String Any)
{-# NOINLINE partitionRegistry #-}
partitionRegistry = unsafePerformIO $ newMVar M.empty

-- | Get the globally unique partition handle for this partition type.
partition :: forall p . Typeable p => IO (Partition p)
partition = do
    let typ = show $ typeOf (undefined :: p)
    modifyMVar partitionRegistry $ \reg ->
        case M.lookup typ reg of
            Just part -> return (reg, unsafeCoerce part)
            Nothing   -> do
                part <- createPartition
                return (M.insert typ (unsafeCoerce part) reg, part)

createPartition :: IO (Partition p)
createPartition = do
    ch <- newChan
    forkIO $ forever $ do
        (task, onCompletion) <- readChan ch
        let loop = do
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
        runStateT loop $ ReactiveState {
                asQueue1 = Seq.singleton task,
                asQueue2 = M.empty,
                asFinal = return ()
            }
        onCompletion

    nextNodeIDRef <- newIORef (NodeID 0)
    return $ Partition {
            paRun = \task onCompletion -> writeChan ch (task, onCompletion),
            paNextNodeID = nextNodeIDRef
        }

-- | Execute the specified 'Reactive' within a new transaction, firing it off without
-- waiting for it to complete. It will be queued for executing on the FRP thread
-- for the selected partition.
--
-- State changes to 'hold' values occur after processing of the transaction is complete.
asynchronously :: Typeable p => Reactive p () -> IO ()
asynchronously task = do
    part <- partition
    paRun part task (return ())

-- | Execute the specified 'Reactive' within a new transaction, blocking the caller
-- until all resulting processing is complete and all callbacks have been called.
--
-- State changes to 'hold' values occur after processing of the transaction is complete.
synchronously :: Typeable p => Reactive p a -> IO a
synchronously task = do
    mvOutput <- newEmptyMVar
    mvCompleted <- newEmptyMVar
    part <- partition
    paRun part (task >>= ioReactive . putMVar mvOutput) (putMVar mvCompleted ())
    takeMVar mvCompleted
    takeMVar mvOutput

data Listen p a = Listen { runListen_ :: Maybe (MVar (Node p)) -> (a -> Reactive p ()) -> Reactive p (IO ()) }

runListen :: Listen p a -> Maybe (MVar (Node p)) -> (a -> Reactive p ()) -> Reactive p (IO ())
{-# NOINLINE runListen #-}
runListen l mv handle = do
    o <- runListen_ l mv handle
    _ <- ioReactive $ evaluate l
    return o

-- | A stream of events. The individual firings of events are called \'event occurrences\'.
data Event p a = Event {  -- Must be data not newtype, because we need to attach finalizers to it
        -- | Listen for event occurrences on this event, to be handled by the specified
        -- handler. The returned action is used to unregister the listener.
        getListenRaw :: Reactive p (Listen p a),
        evCacheRef   :: IORef (Maybe (Listen p a))
    }

-- | An event that never fires.
never :: Event p a
never = Event {
        getListenRaw = return $ Listen $ \_ _ -> return (return ()), 
        evCacheRef   = unsafePerformIO $ newIORef Nothing
    }

-- | Unwrap an event's listener machinery.
getListen :: Event p a -> Reactive p (Listen p a)
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
linkedListen :: Event p a -> Maybe (MVar (Node p)) -> (a -> Reactive p ()) -> Reactive p (IO ())
linkedListen ev mMvTarget handle = do
    l <- getListen ev
    runListen l mMvTarget handle

-- | Variant of 'listenIO' that allows you to initiate more activity in the current
-- transaction. Useful for implementing new primitives.
listen :: Event p a -> (a -> Reactive p ()) -> Reactive p (IO ())
listen ev handle = linkedListen ev Nothing handle

-- | Listen for firings of this event. The returned @IO ()@ is an IO action
-- that unregisters the listener. This is the observer pattern.
listenIO :: Event p a -> (a -> IO ()) -> Reactive p (IO ())
listenIO ev handle = listen ev (ioReactive . handle)

data Observer p a = Observer {
        obNextID    :: ID,
        obListeners :: Map ID (a -> Reactive p ()),
        obFirings   :: [a]
    }

data Node p = Node {
        noID        :: NodeID,
        noRank      :: Int64,
        noListeners :: Map ID (MVar (Node p))
    }

newNode :: forall p . Typeable p => IO (MVar (Node p))
newNode = do
    part <- partition :: IO (Partition p)
    nodeID <- readIORef (paNextNodeID part)
    modifyIORef (paNextNodeID part) succ
    newMVar (Node nodeID 0 M.empty)

wrap :: (Maybe (MVar (Node p)) -> (a -> Reactive p ()) -> Reactive p (IO ())) -> IO (Listen p a)
{-# NOINLINE wrap #-}
wrap l = return (Listen l)

touch :: Listen p a -> IO ()
{-# NOINLINE touch #-}
touch l = evaluate l >> return ()

linkNode :: MVar (Node p) -> ID -> MVar (Node p) -> IO ()
linkNode mvNode iD mvTarget = do
    no <- readMVar mvNode
    ensureBiggerThan S.empty mvTarget (noRank no)
    modifyMVar_ mvNode $ \no -> return $
        no { noListeners = M.insert iD mvTarget (noListeners no) }

ensureBiggerThan :: Set NodeID -> MVar (Node p) -> Int64 -> IO ()
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

unlinkNode :: MVar (Node p) -> ID -> IO ()
unlinkNode mvNode iD = do
    modifyMVar_ mvNode $ \no -> return $
        no { noListeners = M.delete iD (noListeners no) }

-- | Returns a 'Listen' for registering listeners, and a push action for pushing
-- a value into the event.
newEventImpl :: forall p a . Typeable p => IO (Listen p a, a -> Reactive p (), MVar (Node p))
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
newEventLinked :: Typeable p => IO (Event p a, a -> Reactive p (), MVar (Node p))
newEventLinked = do
    (listen, push, mvNode) <- newEventImpl
    cacheRef <- newIORef Nothing
    let ev = Event {
                getListenRaw = return listen,
                evCacheRef = cacheRef
            }
    return (ev, push, mvNode)

-- | Returns an event, and a push action for pushing a value into the event.
newEvent :: Typeable p => IO (Event p a, a -> Reactive p ())
newEvent = do
    (ev, push, _) <- newEventLinked
    return (ev, push)

instance Functor (Event p) where
    f `fmap` Event getListen cacheRef = Event getListen' cacheRef
      where
        cacheRef = unsafePerformIO $ newIORef Nothing
        getListen' = do
            return $ Listen $ \mMvNode handle -> do
                l <- getListen
                runListen l mMvNode (handle . f)

-- | Merge two streams of events of the same type.
--
-- In the case where two event occurrences are simultaneous (i.e. both
-- within the same transaction), both will be delivered in the same
-- transaction.
--
-- The order is not defined, because simultaneous events should be considered
-- to be order-agnostic.
merge :: Typeable p => Event p a -> Event p a -> Event p a
merge ea eb = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        l1 <- getListen ea
        l2 <- getListen eb                                
        (l, push, mvNode) <- ioReactive newEventImpl
        unlistener1 <- unlistenize $ runListen l1 (Just mvNode) push
        unlistener2 <- unlistenize $ runListen l2 (Just mvNode) push
        (finalerize unlistener1 <=< finalerize unlistener2) l

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
mergeWith :: Typeable p => (a -> a -> a) -> Event p a -> Event p a -> Event p a
mergeWith f ea eb = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        l1 <- getListen ea
        l2 <- getListen eb
        (l, push, mvNode) <- ioReactive newEventImpl
        outRef <- ioReactive $ newIORef Nothing
        let process a = do
                mOut <- ioReactive $ readIORef outRef
                ioReactive $ modifyIORef outRef $ \mOut -> Just $ case mOut of
                    Just out -> f out a
                    Nothing  -> a
                when (isNothing mOut) $ schedulePriority2 (Just mvNode) $ do
                    Just out <- ioReactive $ readIORef outRef
                    ioReactive $ writeIORef outRef Nothing
                    push out
        unlistener1 <- unlistenize $ runListen l1 (Just mvNode) process
        unlistener2 <- unlistenize $ runListen l2 (Just mvNode) process
        (finalerize unlistener1 <=< finalerize unlistener2) l

-- | Unwrap Just values, and discard event occurrences with Nothing values.
justE :: Typeable p => Event p (Maybe a) -> Event p a
justE ema = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l', push, mvNode) <- ioReactive newEventImpl
        l <- getListen ema
        unlistener <- unlistenize $ runListen l (Just mvNode) $ \ma -> case ma of
            Just a -> push a
            Nothing -> return ()
        finalerize unlistener l'

-- | Only keep event occurrences for which the predicate is true.
filterE :: Typeable p => (a -> Bool) -> Event p a -> Event p a
filterE pred = justE . ((\a -> if pred a then Just a else Nothing) <$>)

-- | A time-varying value, American spelling.
type Behavior p a = Behaviour p a

-- | A time-varying value, British spelling.
data Behaviour p a = Behaviour {
        -- | Internal: Extract the underlyingEvent event for this behaviour.
        underlyingEvent :: Event p a,
        -- | Obtain the current value of a behaviour.
        sample          :: Reactive p a
    }

instance Functor (Behaviour p) where
    f `fmap` Behaviour underlyingEvent sample =
        Behaviour (f `fmap` underlyingEvent) (f `fmap` sample)

constant :: a -> Behaviour p a
constant a = Behaviour {
        underlyingEvent = never,
        sample = return a
    }

data BehaviourState p a = BehaviourState {
        bsCurrent :: a,
        bsUpdate  :: Maybe a
    }

-- | Add a finalizer to an event.
finalizeEvent :: Event p a -> IO () -> Event p a
{-# NOINLINE finalizeEvent #-}
finalizeEvent ea unlisten = Event gl (evCacheRef ea)
  where
    gl = do
        l <- getListen ea
        ioReactive $ finalizeListen l unlisten

-- | Add a finalizer to a listener.
finalizeListen :: Listen p a -> IO () -> IO (Listen p a)
{-# NOINLINE finalizeListen #-}
finalizeListen l unlisten = do
    addFinalizer l unlisten
    return l

newtype Unlistener = Unlistener (MVar (Maybe (IO ())))

-- | Listen to an input event/behaviour and return an 'Unlistener' that can be
-- attached to an output event using 'finalerize'.
unlistenize :: Reactive p (IO ()) -> Reactive p Unlistener
unlistenize doListen = do
    unlistener@(Unlistener ref) <- newUnlistener
    schedulePriority1 $ do
        mOldUnlisten <- ioReactive $ takeMVar ref
        case mOldUnlisten of
            Just _ -> do
                unlisten <- doListen
                ioReactive $ putMVar ref (Just unlisten)
            Nothing -> ioReactive $ putMVar ref mOldUnlisten
    return unlistener
  where
    newUnlistener :: Reactive p Unlistener
    newUnlistener = Unlistener <$> ioReactive (newMVar (Just $ return ()))

-- | Cause the things listened to with unlistenize to be unlistened when the
-- specified listener is not referenced any more.
finalerize :: Unlistener -> Listen p a -> Reactive p (Listen p a)
finalerize (Unlistener ref) l = ioReactive $ finalizeListen l $ do
    mUnlisten <- takeMVar ref
    fromMaybe (return ()) mUnlisten
    putMVar ref Nothing

-- | Create a behaviour with the specified initial value, that gets updated
-- by the values coming through the event. The \'current value\' of the behaviour
-- is notionally the value as it was 'at the start of the transaction'.
-- That is, state updates caused by event firings get processed at the end of
-- the transaction.
hold :: a -> Event p a -> Reactive p (Behaviour p a)
hold initA ea = do
    bsRef <- ioReactive $ newIORef (BehaviourState initA Nothing)
    unlistener <- unlistenize $ listen ea $ \a -> do
        bs <- ioReactive $ readIORef bsRef
        ioReactive $ writeIORef bsRef $ bs { bsUpdate = Just a }
        when (isNothing (bsUpdate bs)) $ onFinal $ do
            bs <- readIORef bsRef
            let newCurrent = fromJust (bsUpdate bs)
                bs' = newCurrent `seq` BehaviourState newCurrent Nothing
            evaluate bs'
            writeIORef bsRef bs'
    let gl = do
            l <- getListen ea
            finalerize unlistener l
        beh = Behaviour {
                underlyingEvent = Event gl (evCacheRef ea),
                sample = ioReactive $ bsCurrent <$> readIORef bsRef
            }
    return beh

-- | Sample the behaviour at the time of the event firing. Note that the 'current value'
-- of the behaviour that's sampled is the value as at the start of the transaction
-- before any state changes of the current transaction are applied through 'hold's.
attachWith :: Typeable p => (a -> b -> c) -> Event p a -> Behaviour p b -> Event p c
attachWith f ea bb = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l, push, mvNode) <- ioReactive newEventImpl
        unlistener <- unlistenize $ linkedListen ea (Just mvNode) $ \a -> do
            b <- sample bb
            push (f a b)
        finalerize unlistener l

-- | Variant of attachWith defined as /attachWith (,)/ 
attach :: Typeable p => Event p a -> Behaviour p b -> Event p (a,b)
attach = attachWith (,)

-- | Variant of attachWith that throws away the event's value and captures the behaviour's.
tag :: Typeable p => Event p a -> Behaviour p b -> Event p b
tag = attachWith (flip const)

-- | Listen to the value of this behaviour with an initial callback giving
-- the current value. Can get multiple values per transaction, the last of
-- which is considered valid. You would normally want to use 'listenValue',
-- which removes the extra unwanted values.
listenValueRaw :: Behaviour p a -> Maybe (MVar (Node p)) -> (a -> Reactive p ()) -> Reactive p (IO ())
listenValueRaw ba mMvNode handle = do
    a <- sample ba
    handle a
    linkedListen (underlyingEvent ba) mMvNode handle

-- | Queue the specified atomic to run at the end of the priority 2 queue
schedulePriority2 :: Maybe (MVar (Node p))
                  -> Reactive p ()
                  -> Reactive p ()
schedulePriority2 mMvNode task = do
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
tidy :: (Maybe (MVar (Node p)) -> (a -> Reactive p ()) -> Reactive p (IO ()))
      -> Maybe (MVar (Node p)) -> (a -> Reactive p ()) -> Reactive p (IO ())
tidy listen mMvNode handle = do
    aRef <- ioReactive $ newIORef Nothing
    listen mMvNode $ \a -> do
        ma <- ioReactive $ readIORef aRef
        ioReactive $ writeIORef aRef (Just a)
        when (isNothing ma) $ schedulePriority2 mMvNode $ do
            Just a <- ioReactive $ readIORef aRef
            ioReactive $ writeIORef aRef Nothing
            handle a

-- | Listen to the value of this behaviour with a guaranteed initial callback
-- giving the current value, followed by callbacks for any updates. 
linkedListenValue :: Behaviour p a -> Maybe (MVar (Node p)) -> (a -> Reactive p ()) -> Reactive p (IO ())
linkedListenValue ba = tidy (listenValueRaw ba)

-- | Variant of 'listenValueIO' that allows you to initiate more activity in the current
-- transaction. Useful for implementing new primitives.
listenValue :: Behaviour p a -> (a -> Reactive p ()) -> Reactive p (IO ())
listenValue ba = linkedListenValue ba Nothing

-- | Listen to the value of this behaviour with a guaranteed initial callback
-- giving the current value, followed by callbacks for any updates. 
listenValueIO :: Behaviour p a -> (a -> IO ()) -> Reactive p (IO ())
listenValueIO ba handle = listenValue ba (ioReactive . handle)

eventify :: Typeable p => (Maybe (MVar (Node p)) -> (a -> Reactive p ()) -> Reactive p (IO ())) -> Event p a
eventify listen = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l, push, mvNode) <- ioReactive newEventImpl
        unlistener <- unlistenize $ listen (Just mvNode) push
        finalerize unlistener l

-- | An event that fires once for the current value of the behaviour, and then
-- for all changes that occur after that.
valueEvent :: Typeable p => Behaviour p a -> Event p a
valueEvent ba = eventify (linkedListenValue ba)

instance Typeable p => Applicative (Behaviour p) where
    pure = constant
    Behaviour u1 s1 <*> Behaviour u2 s2 = Behaviour u s
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
            (finalerize unlistener1 <=< finalerize unlistener2) l
        s = ($) <$> s1 <*> s2

-- | Let event occurrences through only when the behaviour's value is True.
-- Note that the behaviour's value is as it was at the start of the transaction,
-- that is, no state changes from the current transaction are taken into account.
gate :: Typeable p => Event p a -> Behaviour p Bool -> Event p a
gate ea = justE . attachWith (\a b -> if b then Just a else Nothing) ea

-- | Transform an event with a generalized state loop (a mealy machine). The function
-- is passed the input and the old state and returns the new state and output value.
collectE :: Typeable p => (a -> s -> (b, s)) -> s -> Event p a -> Reactive p (Event p b)
collectE f z ea = do
    rec
        s <- hold z es
        let ebs = attachWith f ea s
            eb = fst <$> ebs
            es = snd <$> ebs
    return eb

-- | Transform a behaviour with a generalized state loop (a mealy machine). The function
-- is passed the input and the old state and returns the new state and output value.
collect :: Typeable p => (a -> s -> (b, s)) -> s -> Behaviour p a -> Reactive p (Behaviour p b)
collect f zs bea = do
    let ea = eventify . tidy . linkedListen $ underlyingEvent bea
    za <- sample bea
    let (zb, zs') = f za zs
    rec
        bs <- hold (zb, zs') ebs
        let ebs = attachWith f ea (snd <$> bs)
    return (fst <$> bs)

-- | Accumulate on input event, outputting the new state each time.
accumE :: Typeable p => (a -> s -> s) -> s -> Event p a -> Reactive p (Event p s) 
accumE f z ea = do
    rec
        let es = attachWith f ea s
        s <- hold z es
    return es

-- | Accumulate on input event, holding state.
accum :: Typeable p => (a -> s -> s) -> s -> Event p a -> Reactive p (Behaviour p s)
accum f z ea = do
    rec
        s <- hold z (attachWith f ea s)
    return s

-- | Count event occurrences, starting with 1 for the first occurrence.
countE :: Typeable p => Event p a -> Reactive p (Event p Int)
countE = accumE (+) 0 . (const 1 <$>)

-- | Count event occurrences, giving a behaviour that starts with 0 before the first occurrence.
count :: Typeable p => Event p a -> Reactive p (Behaviour p Int)
count = hold 0 <=< countE

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

-- | Unwrap an event inside a behaviour to give a time-varying event implementation.
switchE :: Typeable p => Behaviour p (Event p a) -> Event p a
switchE bea = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    unlistensRef = unsafePerformIO $ newIORef M.empty
    gl = do
        -- assign ID numbers to the incoming events
        beaId <- collect (\ea nxtID -> ((ea, nxtID), succ nxtID)) (0 :: ID) bea
        (l, push, mvNode) <- ioReactive newEventImpl
        unlistener1 <- unlistenize $ linkedListenValue beaId (Just mvNode) $ \(ea, iD) -> do
            let filtered = justE $ attachWith (\a activeID ->
                        if activeID == iD
                            then Just a
                            else Nothing
                    ) ea (snd <$> beaId)
            unlisten2 <- listen filtered $ \a -> do
                push a
                ioReactive $ unlistenLessThan unlistensRef iD
            ioReactive $ modifyIORef unlistensRef (M.insert iD unlisten2)
        finalerize unlistener1 l

-- | Unwrap a behaviour inside another behaviour to give a time-varying behaviour implementation.
switch :: Typeable p => Behaviour p (Behaviour p a) -> Reactive p (Behaviour p a)
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

-- | Throw away all event occurrences except for the first one.
once :: Typeable p => Event p a -> Reactive p (Event p a)
once ea = justE <$> collectE (\a active -> (if active then Just a else Nothing, False)) True ea

-- | Execute the specified 'Reactive' action inside an event.
execute :: Typeable p => Event p (Reactive p a) -> Event p a
execute ev = Event gl cacheRef
  where
    cacheRef = unsafePerformIO $ newIORef Nothing
    gl = do
        (l', push, mvNode) <- ioReactive newEventImpl
        unlistener <- unlistenize $ do
            l <- getListen ev
            runListen l (Just mvNode) $ \action -> action >>= push
        finalerize unlistener l'

-- | Cross the specified event over to a different partition.
crossE :: (Typeable p, Typeable q) => Event p a -> Reactive p (Event q a)
crossE epa = do
    (ev, push) <- ioReactive newEvent
    unlisten <- listenIO epa $ asynchronously . push
    return $ finalizeEvent ev unlisten

-- | Cross the specified behaviour over to a different partition.
cross :: (Typeable p, Typeable q) => Behaviour p a -> Reactive p (Behaviour q a)
cross bpa = do
    a <- sample bpa
    ea <- crossE (underlyingEvent bpa)
    ioReactive $ synchronously $ hold a ea

