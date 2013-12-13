{-# LANGUAGE DoRec #-}
-- | Make sure this program runs without leaking memory
import FRP.Sodium
import Data.Maybe
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.DeepSeq
import System.Timeout

-- A truncated version of memory-test-8 that still shows the same problem.

verbose = False

flam :: Event () -> Behavior Int -> Reactive (Behavior (Maybe Int))
flam e time = do
    -- Test that this arrangement...
    --
    --         e --> eStart --> GATE --> HOLD
    --                            ^        |
    --                            |        v
    --                    notRunning <-- mRunning --> * ---> OUT
    --
    -- ...get cleaned up when 'flam' is switched out. The issue is the 
    -- GATE/HOLD cycle at the bottom left. 
    let eStart = snapshot (\() t -> Just t) e time
    rec
        -- Only allow eStart through when we're not already running
        mRunning <- hold Nothing $ eStart `gate` notRunning
        let notRunning = fmap (not . isJust) mRunning
    return mRunning

main = do
    (e, push) <- sync newEvent
    (eFlip, pushFlip) <- sync newEvent
    (time, pushTime) <- sync $ newBehavior 0
    out <- sync $ do
        eInit <- flam e time
        eFlam <- hold eInit (execute ((const $ flam e time) <$> eFlip))
        switch eFlam
    kill <- sync $ listen (value out) $ \x ->
        if verbose then print x else (evaluate (rnf x) >> return ())
    let loop t = do
            sync $ pushTime t
            sync $ push ()
            when (t `mod` 18 == 0) $ sync $ pushFlip ()
            loop (t+1)
    timeout 4000000 $ loop 0
    kill

