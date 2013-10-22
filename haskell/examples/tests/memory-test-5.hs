-- | Make sure this program runs without leaking memory
import FRP.Sodium
import Control.Applicative
import Control.Exception
import Control.Monad
import System.Timeout

verbose = False

main = do
    (et, _) <- sync newEvent
    (eChange, pushC) <- sync $ newEvent
    out <- sync $ hold 0 eChange
    kill <- sync $ listen (value out) $ \x ->
        if verbose then print (x :: Int) else (evaluate x >> return ())
    timeout 4000000 $ forM_ [0..] $ \i -> do
        sync $ pushC i
    kill

