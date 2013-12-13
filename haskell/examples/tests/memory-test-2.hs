{-# LANGUAGE DoRec #-}
-- | Make sure this program runs without leaking memory
import FRP.Sodium
import Control.Applicative
import Control.Exception
import System.Timeout

data Source = Source { unSource :: Reactive (Behaviour (Int, Int), Event Source) }

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
    oout <- sync $ do
        let newSource = (\tens -> Source $ do
                    let out = ((,) tens) <$> t
                    return (out, newSource)
                ) <$> changeTens
            initPair = (((,) 0) <$> t, newSource)
        rec
            bPair <- hold initPair eSwitch
            let eSwitch = execute $ unSource <$> switchE (snd <$> bPair)
        return (fst <$> bPair)
    out <- sync $ switch oout
    kill <- sync $ listen (value out) $ \x ->
        if verbose then print x else (evaluate x >> return ())
    timeout 4000000 $ mapM_ (sync . pushT) [0..]
    kill

