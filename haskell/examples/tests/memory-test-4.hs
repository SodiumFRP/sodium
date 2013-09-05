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
    out <- sync $ do
        oout <- hold et $ (\_ -> et) <$> eChange
        return $ switchE oout
    kill <- sync $ listen out $ \x ->
        if verbose then print (x :: Int) else (evaluate x >> return ())
    timeout 4000000 $ forM_ [0..] $ \i -> do
        sync $ pushC ()
    kill

