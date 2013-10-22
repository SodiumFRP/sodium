-- | Make sure this program runs without leaking memory
import FRP.Sodium
import Control.Applicative
import Control.Exception
import Control.Monad
import System.Timeout

verbose = False

main = do
    (et, _) <- sync newEvent
    t <- sync $ hold (0 :: Int) et
    (eChange, pushC) <- sync $ newEvent
    out <- sync $ do
        oout <- hold t $ (\_ -> t) <$> eChange
        switch oout
    kill <- sync $ listen (value out) $ \x ->
        if verbose then print x else (evaluate x >> return ())
    timeout 4000000 $ forM_ [0..] $ \i -> do
        sync $ pushC ()
    kill

