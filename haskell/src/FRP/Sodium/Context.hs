{-# LANGUAGE TypeFamilies, DoRec, FlexibleContexts, ScopedTypeVariables #-}
-- | Generalization of the Sodium API to allow for parallel processing.
module FRP.Sodium.Context where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix

class (
          Applicative (Reactive r),
          Monad (Reactive r),
          MonadFix (Reactive r),
          Functor (Event r),
          Applicative (Behavior r)
      ) =>
      Context r where
    -- | A monad for transactional reactive operations. Execute it from 'IO' using 'sync'.
    data Reactive r :: * -> *
    -- | A stream of events. The individual firings of events are called \'event occurrences\'.
    data Event r :: * -> *
    -- | A time-varying value, American spelling.
    data Behavior r :: * -> *
    -- | Execute the specified 'Reactive' within a new transaction, blocking the caller
    -- until all resulting processing is complete and all callbacks have been called.
    -- This operation is thread-safe, so it may be called from any thread.
    --
    -- State changes to 'hold' values occur after processing of the transaction is complete.
    sync          :: Reactive r a -> IO a
    -- Lift an arbitrary IO action into a 'Reactive'.
    ioReactive    :: IO a -> Reactive r a
    -- | Returns an event, and a push action for pushing a value into the event.
    newEvent      :: Reactive r (Event r a, a -> Reactive r ())
    -- | Listen for firings of this event. The returned @IO ()@ is an IO action
    -- that unregisters the listener. This is the observer pattern.
    --
    -- To listen to a 'Behavior' use @listen (values b) handler@
    listen        :: Event r a -> (a -> IO ()) -> Reactive r (IO ())
    -- | An event that never fires.
    never         :: Event r a
    -- | Merge two streams of events of the same type.
    --
    -- In the case where two event occurrences are simultaneous (i.e. both
    -- within the same transaction), both will be delivered in the same
    -- transaction. If the event firings are ordered for some reason, then
    -- their ordering is retained. In many common cases the ordering will
    -- be undefined.
    merge         :: Event r a -> Event r a -> Event r a
    -- | Unwrap Just values, and discard event occurrences with Nothing values.
    filterJust    :: Event r (Maybe a) -> Event r a
    -- | Create a behavior with the specified initial value, that gets updated
    -- by the values coming through the event. The \'current value\' of the behavior
    -- is notionally the value as it was 'at the start of the transaction'.
    -- That is, state updates caused by event firings get processed at the end of
    -- the transaction.
    hold          :: a -> Event r a -> Reactive r (Behavior r a)
    -- | An event that gives the updates for the behavior. It doesn't do any equality
    -- comparison as the name might imply.
    changes       :: Behavior r a -> Event r a
    -- | An event that is guaranteed to fire once when you listen to it, giving
    -- the current value of the behavior, and thereafter behaves like 'changes',
    -- firing for each update to the behavior's value.
    values        :: Behavior r a -> Event r a
    -- | Sample the behavior at the time of the event firing. Note that the 'current value'
    -- of the behavior that's sampled is the value as at the start of the transaction
    -- before any state changes of the current transaction are applied through 'hold's.
    snapshotWith  :: (a -> b -> c) -> Event r a -> Behavior r b -> Event r c
    -- | Unwrap an event inside a behavior to give a time-varying event implementation.
    switchE       :: Behavior r (Event r a) -> Event r a
    -- | Unwrap a behavior inside another behavior to give a time-varying behavior implementation.
    switch        :: Behavior r (Behavior r a) -> Reactive r (Behavior r a)
    -- | Execute the specified 'Reactive' action inside an event.
    execute       :: Event r (Reactive r a) -> Event r a
    -- | Obtain the current value of a behavior.
    sample        :: Behavior r a -> Reactive r a
    -- | If there's more than one firing in a single transaction, combine them into
    -- one using the specified combining function.
    --
    -- If the event firings are ordered, then the first will appear at the left
    -- input of the combining function. In most common cases it's best not to
    -- make any assumptions about the ordering, and the combining function would
    -- ideally be commutative.
    coalesce      :: (a -> a -> a) -> Event r a -> Event r a
    -- | Throw away all event occurrences except for the first one.
    once          :: Context r => Event r a -> Event r a

-- | A time-varying value, British spelling.
type Behaviour r a = Behavior r a

-- | Create a new 'Behavior' along with an action to push changes into it.
-- American spelling.
newBehavior :: forall r a . Context r =>
               a  -- ^ Initial behavior value
            -> Reactive r (Behavior r a, a -> Reactive r ())
newBehavior initA = do
    (ev, push) <- newEvent
    beh <- hold initA ev
    return (beh, push)

-- | Create a new 'Behavior' along with an action to push changes into it.
-- British spelling.
newBehaviour :: forall r a . Context r =>
               a  -- ^ Initial behavior value
            -> Reactive r (Behavior r a, a -> Reactive r ())
newBehaviour = newBehavior

-- | Merge two streams of events of the same type, combining simultaneous
-- event occurrences.
--
-- In the case where multiple event occurrences are simultaneous (i.e. all
-- within the same transaction), they are combined using the same logic as
-- 'coalesce'.
mergeWith :: Context r => (a -> a -> a) -> Event r a -> Event r a -> Event r a
mergeWith f ea eb = coalesce f $ merge ea eb

-- | Only keep event occurrences for which the predicate returns true.
filterE :: Context r => (a -> Bool) -> Event r a -> Event r a
filterE pred = filterJust . ((\a -> if pred a then Just a else Nothing) <$>)

-- | Variant of snapshotWith that throws away the event's value and captures the behavior's.
snapshot :: Context r => Event r a -> Behavior r b -> Event r b
snapshot = snapshotWith (flip const)

-- | Let event occurrences through only when the behavior's value is True.
-- Note that the behavior's value is as it was at the start of the transaction,
-- that is, no state changes from the current transaction are taken into account.
gate :: Context r => Event r a -> Behavior r Bool -> Event r a
gate ea = filterJust . snapshotWith (\a b -> if b then Just a else Nothing) ea

-- | Transform an event with a generalized state loop (a mealy machine). The function
-- is passed the input and the old state and returns the new state and output value.
collectE :: Context r => (a -> s -> (b, s)) -> s -> Event r a -> Reactive r (Event r b)
collectE f z ea = do
    rec
        s <- hold z es
        let ebs = snapshotWith f ea s
            eb = fst <$> ebs
            es = snd <$> ebs
    return eb

-- | Transform a behavior with a generalized state loop (a mealy machine). The function
-- is passed the input and the old state and returns the new state and output value.
collect :: Context r => (a -> s -> (b, s)) -> s -> Behavior r a -> Reactive r (Behavior r b)
collect f zs bea = do
    let ea = coalesce (flip const) (changes bea)
    za <- sample bea
    let (zb, zs') = f za zs
    rec
        bs <- hold (zb, zs') ebs
        let ebs = snapshotWith f ea (snd <$> bs)
    return (fst <$> bs)

-- | Accumulate on input event, outputting the new state each time.
accumE :: Context r => (a -> s -> s) -> s -> Event r a -> Reactive r (Event r s) 
accumE f z ea = do
    rec
        let es = snapshotWith f ea s
        s <- hold z es
    return es

-- | Accumulate on input event, holding state.
accum :: Context r => (a -> s -> s) -> s -> Event r a -> Reactive r (Behavior r s)
accum f z ea = do
    rec
        s <- hold z (snapshotWith f ea s)
    return s

-- | Count event occurrences, starting with 1 for the first occurrence.
countE :: Context r => Event r a -> Reactive r (Event r Int)
countE = accumE (+) 0 . (const 1 <$>)

-- | Count event occurrences, giving a behavior that starts with 0 before the first occurrence.
count :: Context r => Event r a -> Reactive r (Behavior r Int)
count = hold 0 <=< countE

