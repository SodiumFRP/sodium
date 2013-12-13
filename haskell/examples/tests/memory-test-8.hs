{-# LANGUAGE DoRec #-}
-- | Make sure this program runs without leaking memory
import FRP.Sodium
import Data.Maybe
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.DeepSeq
import System.Timeout

verbose = False

flam :: Event () -> Behavior Int -> Reactive (Behavior (Maybe Int))
flam e time = do
    -- Test that this arrangement...
    --
    --                                           updates time
    --                                                |
    --                                                v
    --                                   eStop <-- SNAPSHOT (timer)
    --                                     |          ^
    --                                     v          |
    --         e --> eStart --> GATE --> HOLD         |
    --                            ^        |          |
    --                            |        v          |
    --                    notRunning <-- mRunning --> * ---> OUT
    --
    -- ...get cleaned up when 'flam' is switched out. The issue is the 
    -- GATE/HOLD cycle at the bottom left. 
    let eStart = snapshot (\() t -> Just t) e time
    rec
        let notRunning = fmap (not . isJust) mRunning
        -- Only allow eStart through when we're not already running
        mRunning <- hold Nothing $ merge (eStart `gate` notRunning) eStop

        -- Stop it when it's been running for 5 ticks.
        let eStop = filterJust $ snapshot (\t mRunning ->
                    case mRunning of
                        Just t0 | (t - t0) >= 5 -> Just Nothing
                        _                       -> Nothing
                ) (updates time) mRunning
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

