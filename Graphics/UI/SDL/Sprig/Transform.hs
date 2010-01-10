-- | A collection of functions for transforming SDL surfaces (rotation, scaling).
module Graphics.UI.SDL.Sprig.Transform 
   (transform, Graphics.UI.SDL.Sprig.Transform.rotate, rotateAA, Flag(..)) where
import Foreign
import Foreign.C
import qualified Data.Bits as B hiding (rotate)
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Utilities
import Graphics.UI.SDL.Color

foreign import ccall unsafe "SPG_Transform"  spgTransform  :: Ptr SurfaceStruct -> Word32 -> Float -> Float -> Float -> Word8 -> IO (Ptr SurfaceStruct)
foreign import ccall unsafe "SPG_Rotate"     spgRotate     :: Ptr SurfaceStruct -> Float -> Word32 -> IO (Ptr SurfaceStruct)
foreign import ccall unsafe "SPG_RotateAA"   spgRotateAA   :: Ptr SurfaceStruct -> Float -> Word32 -> IO (Ptr SurfaceStruct)

-- | Flags used for transform calls
data Flag = AntiAlias -- ^ Antialiasing is applied to smooth the result of transformation
          | SafeBitDepths -- ^ Take care with unusual bit depths
          | ColorKeyTransparency -- ^ enable colorkey accuracy
          | FastTextureMap -- ^ Faster but uglier texture map renderer
          | SlowButAccurate -- ^ Slower but accurate transform
          | AlphaBlending -- ^ Enable alpha blending
          | SurfaceAlpha  
          deriving (Show, Eq)

flagsToWord8 :: [Flag] -> Word8
flagsToWord8 flags = foldl (B..|.) 0 $ map flagToWord8 flags
   where flagToWord8 AntiAlias            = 0x01
         flagToWord8 SafeBitDepths        = 0x02
         flagToWord8 FastTextureMap       = 0x04
         flagToWord8 SlowButAccurate      = 0x08
         flagToWord8 ColorKeyTransparency = 0x10
         flagToWord8 AlphaBlending        = 0x20
         flagToWord8 SurfaceAlpha         = 0x40

-- |Simple transformation of an SDL surface, creating a new surface
transform :: Surface  -- ^ Surface to transform
        ->  Pixel  -- ^ Background color for exposed regions (for rotation)
        ->  Float  -- ^ Rotation angle (degrees/radians specified by control state)
        ->  Float  -- ^ X scale factor
        ->  Float  -- ^ Y scale factor
        -> [Flag]  -- ^ Transformation flags
        -> IO Surface
transform surf (Pixel w) f1 f2 f3 flgs = withForeignPtr surf $ \s-> spgTransform s w f1 f2 f3 (flagsToWord8 flgs) >>= mkFinalizedSurface

-- | Rotation convenience function
rotate :: Surface -> Float -> Pixel -> IO Surface
rotate surf f (Pixel w) = withForeignPtr surf $ \s-> spgRotate s f w >>= mkFinalizedSurface

-- | Rotation convenience function (antialiasing on)
rotateAA :: Surface -> Float -> Pixel -> IO Surface
rotateAA surf f (Pixel w) = withForeignPtr surf $ \s-> spgRotateAA s f w >>= mkFinalizedSurface
