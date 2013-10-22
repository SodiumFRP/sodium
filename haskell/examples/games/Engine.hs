{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls #-}
module Engine where

import FRP.Sodium

import Control.Applicative
import Control.Monad
import Data.List
import Graphics.Rendering.OpenGL as GL hiding (Triangle, Rect, translate)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT hiding (Rect, translate)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import System.Time
import Debug.Trace

import Image

frameRate :: Num a => a
frameRate = 40

type Coord = Double
type Point = (Coord, Coord)
type Vector = (Coord, Coord)
type Rect = (Point, Vector)   -- Central point and size from centre to edge
type Sprite = (Rect, String)

data MouseEvent = MouseDown Point | MouseMove Point | MouseUp Point
    deriving Show

plus :: Point -> Vector -> Point
plus (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

minus :: Point -> Point -> Vector
minus (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)

-- | True if the point is inside the rectangle
inside :: Point -> Rect -> Bool
inside (x, y) ((ox, oy), (wx, wy)) =
    x >= ox - wx && x <= ox + wx &&
    y >= oy - wy && y <= oy + wy

-- | True if the two rectangles overlap
overlaps :: Rect -> Rect -> Bool
overlaps ((x0, y0), (w0, h0)) ((x1, y1), (w1, h1)) =
    let ax0 = x0 - w0
        ay0 = y0 - h0
        ax1 = x0 + w0
        ay1 = y0 + h0
        bx0 = x1 - w1
        by0 = y1 - h1
        bx1 = x1 + w1
        by1 = y1 + h1
    in ax1 > bx0 &&
       ay1 > by0 &&
       ax0 < bx1 &&
       ay0 < by1

-- | Get system time in seconds since the start of the Unix epoch
-- (1 Jan 1970).
getTime :: IO Double
getTime = do
    (TOD sec pico) <- getClockTime
    return $!
        (fromIntegral sec) +
        (fromIntegral pico) / 1000000000000

-- | Game, which takes mouse event and time as input, and a list of sprites to draw
-- as output. Time is updated once per animation frame.
type Game = Event MouseEvent -> Behaviour Double -> Reactive (Behaviour [Sprite])

runGame :: String -> Game -> IO ()
runGame title game = do

    (eMouse, pushMouse) <- sync newEvent
    (eTime, pushTime) <- sync newEvent
    spritesRef <- newIORef []
    unlisten <- sync $ do
        time <- hold 0 eTime
        sprites <- game eMouse time
        listen (value sprites) (writeIORef spritesRef)

    _ <- GLUT.getArgsAndInitialize
    GLUT.initialDisplayMode $= [GLUT.DoubleBuffered]
    GLUT.createWindow title
    GLUT.windowSize $= GLUT.Size 700 500
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    multisample $= Enabled
    shadeModel $= Smooth
    polygonSmooth $= Enabled
    hint PolygonSmooth $= Nicest
    hint LineSmooth $= Nicest
    normalize $= Enabled

    texturesRef <- newIORef M.empty
    t0 <- getTime
    GLUT.displayCallback $= display texturesRef t0 pushTime spritesRef
    let motion (GLUT.Position x y) = do
            pt <- toScreen x y
            sync $ pushMouse (MouseMove pt)
    GLUT.motionCallback $= Just motion
    GLUT.passiveMotionCallback $= Just motion
    GLUT.keyboardMouseCallback $= Just (\key keyState mods pos -> do
            case (key, keyState, pos) of
                (GLUT.MouseButton GLUT.LeftButton, GLUT.Down, GLUT.Position x y) ->
                    sync . pushMouse . MouseDown =<< toScreen x y
                (GLUT.MouseButton GLUT.LeftButton, GLUT.Up,   GLUT.Position x y) ->
                    sync . pushMouse . MouseUp   =<< toScreen x y
                _ -> return ()
        )
    GLUT.addTimerCallback (1000 `div` frameRate) $ repaint
    GLUT.mainLoop
    unlisten
  where
    toScreen :: GLint -> GLint -> IO (Coord, Coord)
    toScreen x y = do
        (_, Size w h) <- get viewport
        let aspect = fromIntegral w / fromIntegral h
            sx = 0.001/aspect
            sy = 0.001
            xx = 2 * ((fromIntegral x / fromIntegral w) - 0.5) / sx
            yy = 2 * (0.5 - (fromIntegral y / fromIntegral h)) / sy
        return (xx, yy)
    repaint = do
        GLUT.postRedisplay Nothing
        GLUT.addTimerCallback (1000 `div` frameRate) $ repaint

    period = 1 / frameRate

    display :: IORef (Map String (TextureImage, TextureObject))
            -> Double
            -> (Double -> Reactive ())
            -> IORef [Sprite]
            -> IO ()
    display texturesRef t0 pushTime spritesRef = do

        t <- subtract t0 <$> getTime
        sync $ pushTime t

        sprites <- readIORef spritesRef

        clearColor $= Color4 0 0 0 (1 :: GLclampf)
        --clearColor $= Color4 0.1 0.1 0.15 (1 :: GLclampf)
        clear [ColorBuffer{-, DepthBuffer-}]
        loadIdentity

        (_, Size w h) <- get viewport
        let aspect = fromIntegral w / fromIntegral h
        scale (0.001/aspect) 0.001 (0.001 :: GLfloat)

        forM_ sprites $ \(((posX, posY),(sizeX, sizeY)),imgFile) -> do
            textures <- readIORef texturesRef
            (TextureImage iWidth iHeight pWidth pHeight _ _, to) <- case imgFile `M.lookup` textures of
                Just (ti, to) -> return (ti, to)
                Nothing       -> do
                    ti <- loadTexture imgFile False
                    to <- createTexture ti
                    modifyIORef texturesRef (M.insert imgFile (ti, to))
                    return $ (ti, to)

            preservingMatrix $ do
                texture Texture2D $= Enabled
                textureBinding Texture2D $= Just to
                GL.translate $ Vector3 (realToFrac posX) (realToFrac posY) (0 :: GLdouble)
                let w2 = realToFrac sizeX :: GLdouble
                    h2 = realToFrac sizeY :: GLdouble
                    cx = realToFrac pWidth / realToFrac iWidth :: GLfloat
                    cy = realToFrac pHeight / realToFrac iHeight :: GLfloat
                renderPrimitive Polygon $ do
                    texCoord $ TexCoord2 0 cy
                    vertex $ Vertex2 (-w2) (-h2)
                    texCoord $ TexCoord2 cx cy
                    vertex $ Vertex2 w2 (-h2)
                    texCoord $ TexCoord2 cx 0
                    vertex $ Vertex2 w2 h2
                    texCoord $ TexCoord2 0 (0 :: GLfloat)
                    vertex $ Vertex2 (-w2) h2
                texture Texture2D $= Disabled
            --translate $ Vector3 0 0 (0.001 :: GLdouble)

        GLUT.swapBuffers
