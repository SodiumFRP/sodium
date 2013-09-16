module FRP.Sodium.IO where

import FRP.Sodium.Context
import FRP.Sodium.Internal

import Control.Concurrent (forkIO)


-- | Execute the specified IO operation asynchronously on a separate thread, and
-- signal the output event in a new transaction upon its completion.
--
-- Caveat: Where 'switch' or 'switchE' is used, when some reactive logic has been
-- switched away, we rely on garbage collection to actually disconnect this logic
-- from any input it may be listening to. With normal Sodium code, everything is
-- pure, so before garbage collection happens, the worst we will get is some wasted
-- CPU cycles. If you are using 'executeAsyncIO'/'executeSyncIO' inside a 'switch'
-- or 'switchE', however, it is possible that logic that has been switched away
-- hasn't been garbage collected yet. This logic /could/ still run, and if it has
-- observable effects, you could see it running after it is supposed to have been
-- switched out. One way to avoid this is to pipe the source event for IO out of the
-- switch, run the 'executeAsyncIO'/'executeSyncIO' outside the switch, and pipe its
-- output back into the switch contents.
executeAsyncIO :: Event Plain (IO a) -> Event Plain a
executeAsyncIO ev = Event gl cacheRef (dep ev)
  where
    cacheRef = unsafeNewIORef Nothing ev
    gl = do
        (l, push, nodeRef) <- ioReactive newEventImpl
        unlistener <- later $ linkedListen ev (Just nodeRef) False $ \action -> do
            ioReactive $ do
                _ <- forkIO $ sync . push =<< action
                return ()
        addCleanup_Listen unlistener l

-- | Execute the specified IO operation synchronously and fire the output event
-- in the same transaction.
--
-- Caveat: See 'executeAsyncIO'.
executeSyncIO :: Event Plain (IO a) -> Event Plain a
executeSyncIO ev = Event gl cacheRef (dep ev)
  where
    cacheRef = unsafeNewIORef Nothing ev
    gl = do
        (l, push, nodeRef) <- ioReactive newEventImpl
        unlistener <- later $ linkedListen ev (Just nodeRef) False $ \action -> do
            push =<< ioReactive action
        addCleanup_Listen unlistener l

