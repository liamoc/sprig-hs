-- | Sprig uses a settings stack to control various settings such as antialiasing, line thickness
-- and blend modes.
--
-- The setting currently at the top of the stack is used by relevant functions
--
-- This module contains functions to manipulate this stack.
module Graphics.UI.SDL.Sprig.Control 
       ( BlendMode (..)
       , lock
       , getLock
       , enableRadians
       , getRadians
       , pushBlend
       , popBlend
       , getBlend
       , pushThickness
       , popThickness
       , getThickness
       , pushAA
       , popAA
       , getAA
       , pushSurfaceAlpha
       , popSurfaceAlpha
       , getSurfaceAlpha 
       ) where

import Data.Int
import Foreign
import Foreign.C


foreign import ccall unsafe "SPG_EnableAutolock" spgLock :: Word8 -> IO ()
foreign import ccall unsafe "SPG_GetAutolock" spgGetLock :: IO Word8
foreign import ccall unsafe "SPG_EnableRadians" spgEnableRadians :: Word8 -> IO ()
foreign import ccall unsafe "SPG_GetRadians" spgGetRadians :: IO Word8 
foreign import ccall unsafe "SPG_PushBlend" spgPushBlend :: Word8 -> IO()
foreign import ccall unsafe "SPG_PopBlend" spgPopBlend :: IO Word8
foreign import ccall unsafe "SPG_GetBlend" spgGetBlend :: IO Word8
foreign import ccall unsafe "SPG_PushThickness" spgPushThickness :: Word16 -> IO ()
foreign import ccall unsafe "SPG_PopThickness" spgPopThickness :: IO Word16 
foreign import ccall unsafe "SPG_GetThickness" spgGetThickness :: IO Word16
foreign import ccall unsafe "SPG_PushAA" spgPushAA :: Word8 -> IO()
foreign import ccall unsafe "SPG_PopAA" spgPopAA :: IO Word8
foreign import ccall unsafe "SPG_GetAA" spgGetAA :: IO Word8
foreign import ccall unsafe "SPG_PushSurfaceAlpha" spgPushSurfaceAlpha :: Word8 -> IO()
foreign import ccall unsafe "SPG_PopSurfaceAlpha" spgPopSurfaceAlpha :: IO Word8
foreign import ccall unsafe "SPG_GetSurfaceAlpha" spgGetSurfaceAlpha :: IO Word8

-- |Enable or disable auto lock
lock :: Bool -> IO ()
lock = spgLock . boolToWord8

-- |Get auto lock state
getLock :: IO Bool
getLock = fmap word8ToBool spgGetLock

-- |Enable or disable radian measures 
-- Eliminates a degrees -> radians conversion, as radians are used internally
-- Defaults to False.
enableRadians :: Bool -> IO ()
enableRadians = spgEnableRadians . boolToWord8

-- |Determine if radians are being used as the angle measure
getRadians :: IO Bool
getRadians = fmap word8ToBool spgGetRadians

-- |Push a blend mode to the control stack
pushBlend :: BlendMode -> IO ()
pushBlend = spgPushBlend . blendModeToWord8

-- |Get the current blend mode 
getBlend :: IO BlendMode
getBlend = fmap word8ToBlendMode spgGetBlend

-- |Pops the most recent blend mode from the stack and returns it.
popBlend :: IO BlendMode
popBlend = fmap word8ToBlendMode spgPopBlend

-- |Push an antialias setting to the control stack 
-- Default state is false
pushAA :: Bool -> IO ()
pushAA = spgPushAA . boolToWord8

-- |Get the current antialias state
getAA :: IO Bool
getAA = fmap word8ToBool spgGetAA

-- |Pop the antialias state from the stack, and return it.
popAA :: IO Bool
popAA = fmap word8ToBool spgPopAA

-- |Pushes a setting to enable or disable surface-alpha mode.
pushSurfaceAlpha :: Bool -> IO ()
pushSurfaceAlpha = spgPushSurfaceAlpha . boolToWord8

-- |Gets the current surface-alpha setting
getSurfaceAlpha :: IO Bool
getSurfaceAlpha = fmap word8ToBool spgGetSurfaceAlpha

-- |Pops the current surface-alpha setting from the stack, and returns it.
popSurfaceAlpha :: IO Bool
popSurfaceAlpha = fmap word8ToBool spgPopSurfaceAlpha

-- |Pushes a line thickness for primitives to the stack
pushThickness :: Int -> IO ()
pushThickness = spgPushThickness . fromIntegral

-- |Gets the current thickness for primitives
getThickness :: IO Int
getThickness = fmap fromIntegral spgGetThickness

-- |Pops the current thickness from the stack, and returns it.
popThickness :: IO Int
popThickness = fmap fromIntegral spgPopThickness
 
word8ToBool :: Word8 -> Bool
word8ToBool 0 = False
word8ToBool n = True

boolToWord8 :: Bool -> Word8
boolToWord8 v = if v then 1 else 0

-- | Blend modes used for blitting and drawing
data BlendMode = DestAlpha -- ^ Blends colors normally and keeps the destination per-pixel alpha.  This is the same as SDL_BlitSurface's blending.
               | SrcAlpha  -- ^ Blends colors and copies the source alpha.
               | CombineAlpha  -- ^ Blends colors and blends the alpha.  This is 'true' blending and lends well to some neat compositing effects.                                                                                                                       
               | CopyNoAlpha  -- ^ Copies the source color, sets the dest alpha to opaque.                                                                                                                                                                             
               | CopySrcAlpha  -- ^ Copies the source color, keeps the dest alpha.                                                                                                                                                                                      
               | CopyDestAlpha  -- ^ Copies the source color and alpha.                                                                                                                                                                                                  
               | CopyCombineAlpha  -- ^ Copies the source color and blends the alpha.                                                                                                                                                                                       
               | CopyAlphaOnly  -- ^ Keeps the dest color, but copies the source alpha.                                                                                                                                                                                  
               | CombineAlphaOnly  -- ^ Keeps the dest color, but blends the alpha.                                                                                                                                                                                         
               | ReplaceColorKey   -- ^ If the destination surface has the SDL_SRCCOLORKEY flag, this replaces the dest colorkey color in the image with the source color and alpha.  This is similar to palette-swapping, but can be used with gradients and other images. 
                 deriving (Show,Eq) 

blendModeToWord8 :: BlendMode -> Word8
blendModeToWord8 DestAlpha = 0
blendModeToWord8 SrcAlpha = 1
blendModeToWord8 CombineAlpha = 2
blendModeToWord8 CopyNoAlpha = 3
blendModeToWord8 CopySrcAlpha = 4
blendModeToWord8 CopyDestAlpha = 5
blendModeToWord8 CopyCombineAlpha = 6
blendModeToWord8 CopyAlphaOnly = 7
blendModeToWord8 CombineAlphaOnly = 8
blendModeToWord8 ReplaceColorKey = 9

word8ToBlendMode :: Word8 -> BlendMode
word8ToBlendMode 0 = DestAlpha 
word8ToBlendMode 1 = SrcAlpha 
word8ToBlendMode 2 = CombineAlpha 
word8ToBlendMode 3 = CopyNoAlpha 
word8ToBlendMode 4 = CopySrcAlpha 
word8ToBlendMode 5 = CopyDestAlpha 
word8ToBlendMode 6 = CopyCombineAlpha 
word8ToBlendMode 7 = CopyAlphaOnly 
word8ToBlendMode 8 = CombineAlphaOnly 
word8ToBlendMode 9 = ReplaceColorKey 
 
