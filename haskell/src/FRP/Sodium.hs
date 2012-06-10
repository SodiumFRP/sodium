-- | Sodium Reactive Programming (FRP) system.
--
-- See the /examples/ directory for test cases and examples.
--
-- The @p@ type parameter determines the /partition/ that your FRP is running
-- on. A thread is automatically created for each partition used in the system based
-- on the unique concrete p type, which must be an instance of Typeable. FRP
-- processing runs on this thread, but 'synchronously' will block the calling thread
-- while it waits for FRP processing to complete.
--
-- In most cases you would just use one concrete partition type for everything, defined
-- like this:
--
-- > {-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
-- > import Data.Typeable
-- >
-- > data M deriving Typeable
--
-- Later, if you want your code to be more parallel, you can add more partitions:
-- The 'cross' and 'crossE' functions are used to move events and behaviours between
-- partitions. The separation thus created allows your FRP logic to be partitioned
-- so that the different partitions can run in parallel, with more relaxed guarantees
-- of consistency between partitions.
--
-- Some functions are pure, and others need to run under the 'Reactive' monad via
-- 'synchronously' or 'asynchronously'. An 'Event' /p (/'Reactive' /p a)/ can be flattened
-- to an 'Event' /p a/ using the 'execute' primitive.
--
-- In addition to the explicit functions in the language, note that you can use
--
--   * Functor on 'Event' and 'Behaviour'
--
--   * Applicative on 'behaviour', e.g. @let bsum = (+) \<$\> ba \<*\> bb@
--
--   * Applicative 'pure' is used to give a constant 'Behaviour'.
--
--   * Recursive do (via DoRec) to make state loops with the @rec@ keyword.
--
-- Here's an example of recursive do to write state-keeping loops. Note that
-- all 'hold's are delayed, so 'attachWith' will capture the /old/ value of the state /s/.
--
-- > {-# LANGUAGE DoRec #-}
-- > -- | Accumulate on input event, outputting the new state each time.
-- > accumE :: Typeable p => (a -> s -> s) -> s -> Event p a -> Reactive p (Event p s) 
-- > accumE f z ea = do
-- >     rec
-- >         let es = attachWith f ea s
-- >         s <- hold z es
-- >     return es
module FRP.Sodium (
        -- * Running FRP code
        Reactive,
        synchronously,
        asynchronously,
        newEvent,
        listenIO,
        listenValueIO,
        -- * FRP core language
        Event,
        Behaviour,
        Behavior,
        never,
        merge,
        mergeWith,
        justE,
        hold,
        valueEvent,
        snapshotWith,
        switchE,
        switch,
        execute,
        sample,
        calm,
        -- * Derived FRP functions
        filterE,
        snapshot,
        gate,
        collectE,
        collect,
        accumE,
        accum,
        countE,
        count,
        once,
        -- * Partitions
        crossE,
        cross
    ) where

import FRP.Sodium.Impl

