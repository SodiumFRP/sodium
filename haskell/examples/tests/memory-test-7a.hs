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

main = do
    (e, push) <- sync newEvent
    (eFlip, pushFlip) <- sync newEvent
    (time, pushTime) <- sync $ newBehavior (0 :: Int)
    out <- sync $ do
        let eInit = snapshot e time
        eFlam <- hold eInit (const (snapshot e time) <$> eFlip)
        return $ switchE eFlam
    kill <- sync $ listen out $ \x ->
        if verbose then print x else (evaluate x >> return ())
    timeout 4000000 $ forM [1..] $ \t -> do
        sync $ pushTime t
        sync $ push ()
        when (t `mod` 18 == 0) $ sync $ pushFlip ()
    kill

