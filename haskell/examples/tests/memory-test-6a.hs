{-# LANGUAGE DoRec #-}
-- | Make sure this program runs without leaking memory
import FRP.Sodium
import Control.Applicative
import Control.Exception
import Control.Monad
import System.Timeout

verbose = False

flam :: Event () -> Reactive (Event Bool)
flam e = do
    rec
        let eToggle = snapshot (\() selected -> not selected) e selected
        selected <- hold False eToggle
    return eToggle

main = do
    (e, push) <- sync newEvent
    out <- sync $ do
        eInit <- flam e
        eFlam <- hold eInit (execute ((const $ flam e) <$> e))
        return $ switchE eFlam
    kill <- sync $ listen out $ \x ->
        if verbose then print x else (evaluate x >> return ())
    timeout 4000000 $ forever $ sync $ push ()
    kill

