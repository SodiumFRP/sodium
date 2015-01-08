{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, EmptyDataDecls, OverloadedStrings, DoRec #-}

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM (currentDocument, currentWindow)
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.DOMWindow
import GHCJS.DOM.HTMLCanvasElement
import GHCJS.DOM.HTMLImageElement
import FRP.Sodium
import Debug.Trace

data Context_
type Context = JSRef Context_

foreign import javascript unsafe "$1.getContext($2)"
    getContext :: HTMLCanvasElement -> JSString -> IO Context

foreign import javascript unsafe "$1.fillStyle = $2;"
    setFillStyle :: Context -> JSString -> IO ()

foreign import javascript unsafe "$1.fillRect($2,$3,$4,$5);"
    fillRect :: Context -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.drawImage($2,$3,$4,$5,$6);"
    drawImage_ :: Context -> HTMLImageElement -> Int -> Int -> Int -> Int -> IO ()

drawImage :: Context -> HTMLImageElement -> ((Int, Int), (Int, Int)) -> IO ()
drawImage ctx img ((x,y),(w,h)) = drawImage_ ctx img x y w h

foreign import javascript unsafe "$1.innerWidth"
    innerWidth :: DOMWindow -> IO Int

foreign import javascript unsafe "$1.innerHeight"
    innerHeight :: DOMWindow -> IO Int

foreign import javascript unsafe "new Image()"
    newImage :: IO HTMLImageElement

foreign import javascript unsafe "$1.src = $2;"
    setSrc :: HTMLImageElement -> JSString -> IO ()

loadImage :: JSString -> IO HTMLImageElement
loadImage fn = do
    i <- newImage
    setSrc i fn
    return i

resize :: HTMLCanvasElement -> DOMWindow -> IORef (Int, Int) -> IO ()
resize canvas win sizeRef = do
    wd <- innerWidth win
    ht <- innerHeight win
    htmlCanvasElementSetWidth canvas wd
    htmlCanvasElementSetHeight canvas ht
    putStrLn $ "canvas is "++show wd ++ "x"++ show ht
    writeIORef sizeRef (wd, ht)

data Resources = Resources {
        rsEarth   :: HTMLImageElement,
        rsLander  :: HTMLImageElement,
        rsLanderCrashed :: HTMLImageElement,
        rsSurface :: HTMLImageElement,
        rsThrust  :: HTMLImageElement
    }

lunarLander :: Event Bool -> Reactive (Behavior Bool, Behavior Bool, Behavior Double)
lunarLander eThrust = do
    thrust <- hold False eThrust
    accel <- accum 0 $ fmap (\t ->
        if t
            then (2*gravity+)
            else subtract gravity) eThrust
    vel <- accum 0 $ fmap (+) $ updates $ accel
    pos <- accum 100 $ fmap (\x -> max 0 . (+x)) $ updates $ vel
    rec
        touched <- hold False $ fmap (const True) eTouched
        let eTouched = filterJust $ snapshot (\_ (pos, touched) ->
                    if not touched && pos <= 0 then Just () else Nothing) eThrust
                $ liftA2 (,) pos touched
    -- Make velocity go to zero on impact
    let vel' = liftA2 (\t v -> if t then 0 else v) touched vel
    crashed <- hold False $
        filterJust $ snapshot (\_ vel ->
            if trace (show vel) $ abs vel >= threshold then Just True else Nothing
         ) eTouched vel'
    return (crashed, thrust, pos)
  where
    gravity = 0.001
    threshold = 0.5

main = do
    Just doc <- currentDocument
    Just win <- currentWindow
    Just canvas0 <- documentGetElementById doc ("myCanvas" :: JSString)
    let canvas = castToHTMLCanvasElement canvas0
    sizeRef <- newIORef (0,0)
    resize canvas win sizeRef

    rs <- Resources <$> loadImage "earth.png"
                    <*> loadImage "lander.png"
                    <*> loadImage "lander-crashed.png"
                    <*> loadImage "surface.png"
                    <*> loadImage "thrust.png"

    thrustRef <- newIORef False

    elementOnmousedown canvas $ liftIO $ writeIORef thrustRef True
    elementOnmouseup canvas $ liftIO $ writeIORef thrustRef False
    elementOntouchstart canvas $ liftIO $ writeIORef thrustRef True
    elementOntouchend canvas $ liftIO $ writeIORef thrustRef False

    (sendThrust, sendRestart, crashed, thrust, pos) <- sync $ do
        (eRestart, sendRestart) <- newEvent
        (eThrust, sendThrust) <- newEvent
        outputs0 <- lunarLander eThrust
        ooutputs <- hold outputs0 $ execute $ fmap (\() ->
            lunarLander eThrust) eRestart
        crashed <- switch $ fmap (\(a, _, _) -> a) ooutputs
        thrust  <- switch $ fmap (\(_, b, _) -> b) ooutputs
        pos     <- switch $ fmap (\(_, _, c) -> c) ooutputs
        return (sendThrust, sendRestart, crashed, thrust, pos)

    let surfaceRatio = 408 / 102 :: Double
        groundYFrac = 22 / 102 :: Double
        earthRatio   = 58 / 59 :: Double
        landerYFrac = 140 / 144 :: Double
        landerRatio = 141 / 144 :: Double
        draw = do
            ctx <- getContext canvas "2d"
            (wd, ht) <- readIORef sizeRef
            setFillStyle ctx "#FFFFFF"
            fillRect ctx 0 0 wd ht
            let cx = wd `div` 2
                cy = ht `div` 2
                ext = min wd ht `div` 2
            let surfaceW = ext * 2
                surfaceH = round $ fromIntegral surfaceW / surfaceRatio
                groundY = ht - round (groundYFrac * fromIntegral surfaceW / surfaceRatio)
            drawImage ctx (rsSurface rs) ((cx - ext, ht - surfaceH), (surfaceW, surfaceH))
            let earthW = ext `div` 3
                earthH = round $ fromIntegral earthW / earthRatio
            drawImage ctx (rsEarth rs)   ((wd - earthW * 2, earthW),(earthW,earthH))
            (t, p, c) <- sync $ do
                t <- sample thrust
                p <- sample pos
                c <- sample crashed
                return (t, p, c)
            print p
            let landerW = ext * 3 `div` 5
                landerH = round $ fromIntegral landerW / landerRatio
                landerRange = ht - (ht - groundY) * 2 - landerH
                landerYOffset = round $ landerYFrac * fromIntegral landerH
                landerX = cx - ext `div` 2
                landerY = groundY - round ((if c then 0 else p) * fromIntegral landerRange / 100) - landerYOffset
            drawImage ctx (if c then rsLanderCrashed rs else rsLander rs)
                          ((landerX, landerY), (landerW, landerH))
            when t $ do
                let thrustW = round (fromIntegral landerW * 32 / 141 :: Double)
                    thrustH = round (fromIntegral landerH * 46 / 144 :: Double)
                    thrustXFrac = 0.40 :: Double
                    thrustYFrac = 0.88 :: Double
                    thrustX = landerX + round (thrustXFrac * fromIntegral landerW)
                    thrustY = landerY + round (thrustYFrac * fromIntegral landerH)
                drawImage ctx (rsThrust rs) ((thrustX, thrustY), (thrustW, thrustH))

    domWindowOnresize win $ liftIO $ resize canvas win sizeRef >> draw

    forever $ do
        t <- readIORef thrustRef
        sync $ sendThrust t
        draw
        threadDelay 100000
