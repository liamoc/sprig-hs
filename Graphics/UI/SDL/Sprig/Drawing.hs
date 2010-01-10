-- | This modules contains functions for miscellaneous drawing operations that are not
-- adequately covered by the Primitives module
module Graphics.UI.SDL.Sprig.Drawing 
   ( getPixel
   , blit
   , replaceColor
   , floodFill
   ) where

import Foreign
import Foreign.C
import Data.Int
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Utilities
import Graphics.UI.SDL.Color

foreign import ccall unsafe "SPG_GetPixel"  spgGetPixel  :: Ptr SurfaceStruct -> Int16 -> Int16 -> IO Word32
-- |Get the color in a surface at a specific location
getPixel :: Surface -> Int -> Int -> IO Pixel
getPixel s x y = withForeignPtr s $ \surf -> fmap Pixel $ spgGetPixel surf (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "SPG_FloodFill" spgFloodFill :: Ptr SurfaceStruct -> Int16 -> Int16 -> Word32 -> IO ()
-- |Fill all color contiguous pixels to the indicated one with the specified color.
floodFill :: Surface -> Int -> Int ->  Pixel -> IO ()
floodFill s x y (Pixel w) = withForeignPtr s $ \surf -> spgFloodFill surf (fromIntegral x) (fromIntegral y) w

foreign import ccall unsafe "SPG_Blit" spgBlit :: Ptr SurfaceStruct -> Ptr Rect -> Ptr SurfaceStruct -> Ptr Rect -> IO CInt

-- |Blit one surface to another. Analogous to SDL's blitSurface function, except aware of Sprig's control stack (blend modes, etc.)
blit :: Surface -> Maybe Rect -> Surface -> Maybe Rect -> IO Bool
blit src srcRect dst dstRect
    = withForeignPtr src $ \srcPtr ->
      withForeignPtr dst $ \dstPtr ->
      maybeWith with srcRect $ \srcRectPtr ->
      maybeWith with dstRect $ \dstRectPtr ->
      intToBool (-1) (fmap fromCInt $ spgBlit srcPtr srcRectPtr dstPtr dstRectPtr)

foreign import ccall unsafe "SPG_ReplaceColor" spgReplaceColor :: Ptr SurfaceStruct -> Ptr Rect -> Ptr SurfaceStruct -> Ptr Rect -> Word32 -> IO (Ptr SurfaceStruct)
-- |Returns a new surface with the given color replaced by the corresponding pixels from the src surface.  This is an effect similar to palette-swapping on 8-bit surfaces, but allows for images and gradients to replace the colors.
replaceColor :: Surface -> Maybe Rect -> Surface -> Maybe Rect -> Pixel -> IO Surface
replaceColor src srcRect dst dstRect (Pixel w)
    = withForeignPtr src $ \srcPtr ->
      withForeignPtr dst $ \dstPtr ->
      maybeWith with srcRect $ \srcRectPtr ->
      maybeWith with dstRect $ \dstRectPtr ->
      spgReplaceColor srcPtr srcRectPtr dstPtr dstRectPtr w >>= mkFinalizedSurface
