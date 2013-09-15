module FRP.Sodium.IO where

import FRP.Sodium.Context
import FRP.Sodium.Internal

import Control.Concurrent (forkIO)


executeAsyncIO :: Event Plain (IO a) -> Event Plain a
executeAsyncIO ev = Event gl cacheRef (dep ev)
  where
    cacheRef = unsafeNewIORef Nothing ev
    gl = do
        (l, push, nodeRef) <- ioReactive newEventImpl
        unlistener <- later $ linkedListen ev (Just nodeRef) False $ \action -> do
            ioReactive $ do
                _ <- forkIO $ do
                    sync . push =<< action
                return ()
        addCleanup_Listen unlistener l

executeSyncIO :: Event Plain (IO a) -> Event Plain a
executeSyncIO ev = Event gl cacheRef (dep ev)
  where
    cacheRef = unsafeNewIORef Nothing ev
    gl = do
        (l, push, nodeRef) <- ioReactive newEventImpl
        unlistener <- later $ linkedListen ev (Just nodeRef) False $ \action -> do
            push =<< ioReactive action
        addCleanup_Listen unlistener l

