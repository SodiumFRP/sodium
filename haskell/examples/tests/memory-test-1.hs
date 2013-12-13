-- | Make sure this program runs without leaking memory
import FRP.Sodium
import Control.Applicative
import Control.Exception
import System.Timeout

verbose = False

main = do
    (et, pushT) <- sync $ newEvent
    t <- sync $ hold 0 et
    let etens = (`div` 10) <$> et
    tens <- sync $ hold 0 etens
    let changeTens = filterJust $ snapshot (\new old ->
            if new /= old
                then Just new
                else Nothing) etens tens
    out <- sync $ do
        oout <- hold (((,) 0) <$> t) $ (\tens -> ((,) tens) <$> t) <$> changeTens
        switch oout
    kill <- sync $ listen (value out) $ \x ->
        if verbose then print x else (evaluate x >> return ())
    timeout 4000000 $ mapM_ (sync . pushT) [0..]
    kill

