{-# LANGUAGE DoRec, GeneralizedNewtypeDeriving #-}
-- Package dependencies:
--     random
--     stb-image
--     OpenGL
--     GLUT
import FRP.Sodium hiding (split)
import Control.Applicative
import Control.Monad.Trans
import Data.Maybe
import Engine
import System.Random

poodleSprite :: Point -> Sprite
poodleSprite pt = ((pt,(120,120)), "poodle.png")

-- | Active poodle logic (which could be made much more interesting).
poodle :: PoodleID
       -> Point
       -> Event MouseEvent
       -> Behaviour Double
       -> Reactive (Behaviour (PoodleID, Sprite))
poodle iD pos@(x0,y0) eMouse time = do
    t0 <- sample time
    let dt = subtract t0 <$> time
        bounce dt =
            let t = snd $ properFraction dt
            in  800 * t + (-800) * t^2
        sprite = (\dt -> (iD, poodleSprite (x0, y0 + bounce dt))) <$> dt
    return sprite

-- | Peel a new item off the list each time the event fires.
peelList :: Event x -> [a] -> Reactive (Behaviour a)
peelList ev xs0 =
    hold (head xs0)
            =<< collectE (\_ (x:xs) -> (x, xs)) (tail xs0) ev

-- | Generate events at random intervals.
randomTimes :: StdGen -> Behaviour Double -> Reactive (Event Double)
randomTimes rng time = do
    -- Infinite list of random intervals from 0.25 to 1.2 seconds.
    let intervals = randomRs (0.25, 1.2) rng
    rec
        tLast <- hold 0 eAppear
        interval <- peelList eAppear intervals
        let eTime = value time
            eAppear = filterJust $ snapshot (\t (tLast, interval) ->
                    if t >= tLast + interval then Just t else Nothing
                ) eTime ((,) <$> tLast <*> interval)
    return eAppear

newtype PoodleID = PoodleID Int deriving (Eq, Enum, Show)
data Action = Create PoodleID (Behaviour (PoodleID, Sprite)) | Destroy PoodleID

poodleGame :: StdGen -> Game
poodleGame rng eMouse time = do

    -- Random times for appearance of new poodles
    let (rng1, rng2_) = split rng
        (rng2, rng3) = split rng2_
    eAppear <- do
        randomTimes rng1 time

    -- Pick a position for each new poodle
    eNewPosition <- do
        -- Infinite list of random poodle positions
        let xs = randomRs (-900, 900) rng2
            ys = randomRs (-900, 500) rng3
        let idsAndPoses = zip [PoodleID 1..] (zip xs ys)
        -- Peel an item off the list for each new poodle
        collectE (\_ ((iD, pos):xs) -> ((iD, pos), xs)) idsAndPoses eAppear

    -- Construct a new active poodle for each new position
    let eCreations = execute $ (\(iD, pos) -> do
                beh <- poodle iD pos eMouse time
                return $ Create iD beh
            ) <$> eNewPosition

    rec
        -- Destroy poodles that are clicked on
        let eDestructions = filterJust $ snapshot (\mev poodles ->
                    case mev of
                        MouseDown clickPos -> listToMaybe
                            [ Destroy iD | (iD, (rect, _)) <- poodles,
                                   clickPos `inside` rect]
                        _ -> Nothing
                ) eMouse poodles

        -- Handle creations and destructions giving a behaviour containing a
        -- list of poodle behaviours.
        poodleBehs <- hold [] =<< collectE (\change poodles ->
                let poodles' = case change of
                        Create iD beh -> (iD, beh) : poodles
                        Destroy iD    -> filter (\(thisID, _) -> iD /= thisID) poodles
                in  (map snd poodles', poodles')
            ) [] (eCreations `merge` eDestructions)

        -- Convert list of behaviours into a behaviour containing a list,
        -- then flatten behaviour within behaviour down to a single behaviour of
        -- poodle sprites.
        poodles <- switch $ foldr (\ba bt -> (:) <$> ba <*> bt) (pure []) <$> poodleBehs

    -- Return poodle sprites without their ids
    return (map snd <$> poodles)

main = do
    rng <- newStdGen
    runGame "Poodle invasion - click the poodles to keep them under control" (poodleGame rng)

