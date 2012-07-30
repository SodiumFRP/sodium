{-# LANGUAGE DeriveDataTypeable #-}
module Image where

import Data.ByteString (ByteString)     
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Graphics.Rendering.OpenGL as GL hiding (RGB, RGBA)
import qualified Graphics.Rendering.OpenGL as GL
import Codec.Image.STB
import Data.Bitmap
import Data.Char
import Control.Exception
import Control.Monad
import Data.Typeable
import Data.Word
import Foreign
import Foreign.C


data Format = RGB | RGBA deriving (Eq, Show)

data TextureImage_ d = TextureImage !Int !Int !Int !Int Format d deriving Show
type TextureImage = TextureImage_ ByteString
data ImageException = ImageException String
    deriving Typeable
instance Exception ImageException
instance Show ImageException where
    showsPrec _ (ImageException err) = ("ImageException: "++) . (err++)

loadTexture :: FilePath
            -> Bool               -- ^ True to invert it
            -> IO TextureImage
loadTexture path invert = do
        eImg <- loadImage path
        img <- case eImg of
            Left err -> throwIO $ ImageException $
                "Failed to load image file "++path++": "++err
            Right img -> return img
        withBitmap img $ \(width, height) components padding imgData0 -> do
            fmt <- case components of
                3 -> return RGB
                4 -> return RGBA
                _ -> throwIO $ ImageException $
                    "Failed to load image file "++path++": we only support RGB images"
            let len = width * height * components
            imgData <- mallocBytes len
            B.memcpy imgData imgData0 (fromIntegral len)
            when invert $ do
                let row = width * bytesPerPixel fmt
                    row_sz = fromIntegral row
                allocaBytes row $ \tempRow -> do
                    forM_ [0..height `div` 2] $ \y -> do
                        let r0 = row * y
                            s0 = row * (height - 1 - y)
                        B.memcpy tempRow (imgData `plusPtr` r0) row_sz
                        B.memcpy (imgData `plusPtr` r0) (imgData `plusPtr` s0) row_sz
                        B.memcpy (imgData `plusPtr` s0) tempRow row_sz
            let tex = TextureImage width height width height fmt imgData
            transparent <- hasTransparency tex
            tex' <- if transparent then return tex else removeAlphaChannel tex
            unsafeTextureToBS tex'

-- | Converts a buffer-based Texture to to a ByteString-based one.  Frees
-- the input byte buffer.
unsafeTextureToBS :: TextureImage_ (Ptr Word8) -> IO TextureImage
unsafeTextureToBS (TextureImage iWidth iHeight pWidth pHeight fmt buf) = do
    let bpp = bytesPerPixel fmt
        sz = bpp * iWidth * iHeight
    bytes <- B.create sz $ \str -> B.memcpy str buf (fromIntegral sz)
    free buf
    return $ TextureImage iWidth iHeight pWidth pHeight fmt bytes

bytesPerPixel :: Format -> Int
bytesPerPixel RGB = 3
bytesPerPixel RGBA = 4

hasTransparency :: TextureImage_ (Ptr Word8) -> IO Bool
hasTransparency (TextureImage _ _ _ _ RGB _) = return False
hasTransparency (TextureImage width height _ _ fmt@RGBA imgData) = h 0
  where
    limit = width * height
    bpp = bytesPerPixel fmt
    h ix | ix >= limit = return False
    h ix = do
        alpha <- peekByteOff imgData (ix * bpp + 3) :: IO Word8
        if alpha < 255
            then return True
            else h (ix + 1)

removeAlphaChannel :: TextureImage_ (Ptr Word8) -> IO (TextureImage_ (Ptr Word8))
removeAlphaChannel tex@(TextureImage _ _ _ _ RGB _) = return tex
removeAlphaChannel (TextureImage iWidth iHeight pWidth pHeight fmt@RGBA imgData) = do
    forM_ [1..limit-1] $ \ix -> do
        r <- peekByteOff imgData (ix * bpp) :: IO Word8
        pokeByteOff imgData (ix * 3) r
        g <- peekByteOff imgData (ix * bpp + 1) :: IO Word8
        pokeByteOff imgData (ix * 3 + 1) g
        b <- peekByteOff imgData (ix * bpp + 2) :: IO Word8
        pokeByteOff imgData (ix * 3 + 2) b
    return (TextureImage iWidth iHeight pWidth pHeight RGB imgData)
  where
    limit = iWidth * iHeight
    bpp = bytesPerPixel fmt

-- | Convert texture data into an OpenGL handle
createTexture :: TextureImage
              -> IO TextureObject
createTexture (TextureImage iWid iHt _ _ fmt imgBS) = do
    [texName] <- genObjectNames 1  -- generate our texture.
    --rowAlignment  Unpack $= 1
    textureBinding Texture2D $= Just texName  -- make our new texture the current texture.
    generateMipmap Texture2D $= Enabled
    B.unsafeUseAsCStringLen imgBS $ \(buf, len) -> do
        let glFmt = case fmt of
                RGB  -> GL.RGB
                RGBA -> GL.RGBA
            pixels = PixelData glFmt UnsignedByte buf
        build2DMipmaps Texture2D RGBA' (fromIntegral iWid) (fromIntegral iHt) pixels
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    return texName

-- | Ensure this label can be a valid filename
cleanLabel :: String -> String
cleanLabel =
    scrunch .
    map (\c -> if (c >= 'a' && c <= 'z') ||
                  (c >= 'A' && c <= 'Z') ||
                  (c >= '0' && c <= '9') then c else '_')
  where
    scrunch ('_':'_':cs) = scrunch ('_':cs)
    scrunch (c:cs) = c:scrunch cs
    scrunch [] = []

