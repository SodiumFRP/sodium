{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, RecursiveDo, GADTs,
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
--   * Applicative 'Control.Applicative.pure' is used to give a constant 'Behavior'.
--
--   * A Monoid instance on 'Event' where 'mempty' = 'never' and 'mappend' = 'merge'
--
--   * Recursive do (using the DoRec language extension) to make state loops with the @rec@ keyword.
--
--   * Data.Traversable.'sequenceA' is useful to convert /[Behavior a]/ into /Behavior [a]/.
--
-- Here's an example of recursive do to write state-keeping loops. Note that
-- all 'hold's are delayed, so 'snapshot' will capture the /old/ value of the state /s/.
--
-- > {-# LANGUAGE DoRec #-}
-- > -- | Accumulate state changes given in the input event.
-- > accum :: Context r => a -> Event r (a -> a) -> Reactive r (Behavior r a)
-- > accum z efa = do
-- >     rec
-- >         s <- hold z $ snapshot ($) efa s
-- >     return s
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
        updates,
        value,
        snapshot,
        switchE,
        switch,
        execute,
        sample,
        coalesce,
        once,
        split,
        -- * Derived FRP functions
        mergeWith,
        filterE,
        gate,
        collectE,
        collect,
        accum,
        -- * Deprecated
        changes,
        values,
        snapshotWith,
        count
    ) where

import Data.Monoid
import Data.Traversable (sequenceA)
import FRP.Sodium.Plain

