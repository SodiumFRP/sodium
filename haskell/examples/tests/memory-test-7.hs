{-# LANGUAGE DoRec #-}
-- | Make sure this program runs without leaking memory
import FRP.Sodium
import Data.Maybe
import Control.Applicative
import Control.Exception
import Control.Monad
import System.Timeout

verbose = False

flam :: Event () -> Behavior Int -> Reactive (Behavior (Maybe Int))
flam e time = do
    rec
        let running = fmap (not . isJust) mRunning
            eStart = snapshotWith (\() t -> Just t) (e `gate` running) time
        mRunning <- hold Nothing $ merge eStart eStop
        let eStop = filterJust $ snapshotWith (\t mRunning ->
                    case mRunning of
                        Just t0 | (t - t0) >= 5 -> Just Nothing
                        _                       -> Nothing
                ) (changes time) mRunning
    return mRunning

main = do
    (e, push) <- sync newEvent
    (eFlip, pushFlip) <- sync newEvent
    (time, pushTime) <- sync $ newBehavior 0
    out <- sync $ do
        eInit <- flam e time
        eFlam <- hold eInit (execute ((const $ flam e time) <$> eFlip))
        switch eFlam
    kill <- sync $ listen (values out) $ \x ->
        if verbose then print x else (evaluate x >> return ())
    timeout 2000000 $ forM [1..] $ \t -> do
        sync $ pushTime t
        sync $ push ()
        when (t `mod` 18 == 0) $ sync $ pushFlip ()
    kill

