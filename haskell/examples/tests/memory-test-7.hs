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

flam :: Event () -> Behavior Int -> Reactive (Behavior Int)
flam e time = do
    let eStart = snapshot (flip const) e time
    -- Only allow eStart through when we're not already running
    hold 0 eStart

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

