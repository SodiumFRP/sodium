{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, DoRec, GADTs,
    TypeFamilies, EmptyDataDecls, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
-- | Sodium Reactive Programming (FRP) system.
--
-- See the /examples/ directory for test cases and examples.
--
-- Some functions are pure, and others need to run under the 'Reactive' monad via
-- 'sync'. An 'Event' /(/'Reactive' /a)/ can be flattened
-- to an 'Event' /a/ using the 'execute' primitive.
--
-- In addition to the explicit functions in the language, note that you can use
--
--   * Functor on 'Event' and 'Behavior'
--
--   * Applicative on 'behaviour', e.g. @let bsum = (+) \<$\> ba \<*\> bb@
--
--   * Applicative 'pure' is used to give a constant 'Behavior'.
--
--   * Recursive do (using the DoRec language extension) to make state loops with the @rec@ keyword.
--
-- Here's an example of recursive do to write state-keeping loops. Note that
-- all 'hold's are delayed, so 'attachWith' will capture the /old/ value of the state /s/.
--
-- > {-# LANGUAGE DoRec #-}
-- > -- | Accumulate on input event, outputting the new state each time.
-- > accumE :: (a -> s -> s) -> s -> Event a -> Reactive (Event s) 
-- > accumE f z ea = do
-- >     rec
-- >         let es = attachWith f ea s
-- >         s <- hold z es
-- >     return es
module FRP.Sodium (
        Plain,
        -- * Running FRP code
        Reactive,
        sync,
        newEvent,
        newBehavior,
        newBehaviour,
        listen,
        -- * FRP core language
        Event,
        Behavior,
        Behaviour,
        never,
        merge,
        filterJust,
        hold,
        changes,
        values,
        snapshotWith,
        switchE,
        switch,
        execute,
        sample,
        coalesce,
        -- * Derived FRP functions
        mergeWith,
        filterE,
        snapshot,
        gate,
        collectE,
        collect,
        accumE,
        accum,
        countE,
        count,
        once
    ) where

import FRP.Sodium.Plain

