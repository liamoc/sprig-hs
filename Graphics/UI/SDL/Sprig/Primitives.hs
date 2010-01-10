-- | This module contains a large collection of drawing primitives for use on SDL surface.
-- They are all sensitive to line thickness, antialiasing and blendmode settings as specified
-- in the control module.
module Graphics.UI.SDL.Sprig.Primitives 
   ( -- * Pixel functions
     pixel                
   , pixelBlend           
     -- * Circle functions
   , circle               
   , circleFilled         
   , circleBlend          
   , circleFilledBlend    
     -- * Line Functions
   , lineH                
   , lineHBlend           
   , lineHFade            
   , lineHTex             
   , lineV                
   , lineVBlend           
   , line                 
   , lineBlend            
   , lineFade             
   , lineFadeBlend        
     -- * Rectangle functions
   , rect                 
   , rectFilled           
   , rectBlend            
   , rectFilledBlend      
   , rectRound            
   , rectRoundBlend       
   , rectRoundFilled      
   , rectRoundFilledBlend 
     -- * Arc functions
   , arc                  
   , arcBlend             
   , arcFilled            
   , arcFilledBlend       
     -- * Ellipse functions
   , ellipse              
   , ellipseBlend         
   , ellipseFilled        
   , ellipseFilledBlend   
   , ellipseArb           
   , ellipseBlendArb      
   , ellipseFilledBlendArb
   , ellipseFilledArb     
     -- * Bezier curve functions
   , bezier               
   , bezierBlend          
   ) where                      
                         
import Foreign as Foreign hiding (new)
import Foreign.C
 
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Utilities (Enum(..), intToBool, toBitmask, fromCInt, toCInt)

foreign import ccall unsafe "SPG_Pixel" spgPixel :: Ptr SurfaceStruct -> Int16 -> Int16 -> Word32 -> IO ()
foreign import ccall unsafe "SPG_PixelBlend" spgPixelBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_LineH" spgLineH :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Word32 -> IO ()
foreign import ccall unsafe "SPG_LineHBlend" spgLineHBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_LineV" spgLineV :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Word32 -> IO ()
foreign import ccall unsafe "SPG_LineVBlend" spgLineVBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_Line" spgLine :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO ()
foreign import ccall unsafe "SPG_LineBlend" spgLineBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_LineFade" spgLineFade :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> Word32 -> IO ()
foreign import ccall unsafe "SPG_LineFadeBlend" spgLineFadeBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> Word8 -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_LineHTex" spgLineHTex :: Ptr SurfaceStruct ->Int16 ->Int16 ->Int16 ->Ptr SurfaceStruct ->Int16 ->Int16 ->Int16 ->Int16 -> IO ()
foreign import ccall unsafe "SPG_LineHFade" spgLineHFade :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Word32 -> Word32 -> IO ()
foreign import ccall unsafe "SPG_Rect" spgRect :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO ()
foreign import ccall unsafe "SPG_RectBlend" spgRectBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_RectFilled" spgRectFilled :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO ()
foreign import ccall unsafe "SPG_RectFilledBlend" spgRectFilledBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_RectRound" spgRectRound :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Float -> Word32 -> IO ()
foreign import ccall unsafe "SPG_RectRoundBlend" spgRectRoundBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Float -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_RectRoundFilled" spgRectRoundFilled :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Float -> Word32 -> IO ()
foreign import ccall unsafe "SPG_RectRoundFilledBlend" spgRectRoundFilledBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Float -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_Arc" spgArc :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Float -> Float -> Word32 -> IO ()
foreign import ccall unsafe "SPG_ArcBlend" spgArcBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Float -> Float -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_ArcFilled" spgArcFilled :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Float -> Float -> Word32 -> IO ()
foreign import ccall unsafe "SPG_ArcFilledBlend" spgArcFilledBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Float -> Float -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_Circle" spgCircle :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Word32 -> IO ()
foreign import ccall unsafe "SPG_CircleBlend" spgCircleBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_CircleFilled" spgCircleFilled :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Word32 -> IO ()
foreign import ccall unsafe "SPG_CircleFilledBlend" spgCircleFilledBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_Ellipse" spgEllipse :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Float -> Word32 -> IO ()
foreign import ccall unsafe "SPG_EllipseBlend" spgEllipseBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Float -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_EllipseFilled" spgEllipseFilled :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Float -> Word32 -> IO ()
foreign import ccall unsafe "SPG_EllipseFilledBlend" spgEllipseFilledBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Float -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_EllipseArb" spgEllipseArb :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Float -> Float -> Word32 -> IO ()
foreign import ccall unsafe "SPG_EllipseBlendArb" spgEllipseBlendArb :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Float -> Float -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_EllipseFilledArb" spgEllipseFilledArb :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Float -> Float -> Word32 -> IO ()
foreign import ccall unsafe "SPG_EllipseFilledBlendArb" spgEllipseFilledBlendArb :: Ptr SurfaceStruct -> Int16 -> Int16 -> Float -> Float -> Float -> Word32 -> Word8 -> IO ()
foreign import ccall unsafe "SPG_Bezier" spgBezier :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word8 -> Word32 -> IO ()
foreign import ccall unsafe "SPG_BezierBlend" spgBezierBlend :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word8 -> Word32 -> Word8 -> IO ()

-- |Set the color of a specific pixel on an SDL surface.
pixel :: Surface   -- ^ SDL destination Surface 
         -> Int   -- ^ X 
         -> Int   -- ^ Y
         -> Pixel -- ^ New color
         -> IO ()

-- |Alter the color of a specific pixel on an SDL surface to the specified color, with alpha blending.
pixelBlend :: Surface -- ^ SDL destination Surface 
            -> Int -- ^ X
            -> Int -- ^ Y
            -> Pixel -- ^ New color
            -> Word8 -- ^ (0-255) Level of alpha blending
            -> IO ()

-- |Draw a horizontal line
lineH :: Surface -- ^ SDL destination surface
       -> Int -- ^ Start X 
       -> Int -- ^ End X
       -> Int -- ^ Y
       -> Pixel -- ^ Color
       -> IO ()

-- |Draw a horizontal line, with alpha blending
lineHBlend :: Surface -> Int -> Int -> Int -> Pixel -> Word8 -> IO ()
-- |Draw a horizontal line with a textured fill                                  
lineHTex :: Surface ->Int ->Int ->Int ->Surface ->Int ->Int ->Int ->Int -> IO () 
-- |Draw a horizontal line with a gradient fill                                                                                  
lineHFade :: Surface -> Int -> Int -> Int -> Pixel -> Pixel -> IO ()             
-- |Draw a vertical line
lineV :: Surface -- ^ SDL destination surface
        ->   Int -- ^ X
        ->   Int -- ^ Start Y
        ->   Int -- ^ End Y
        -> Pixel -- ^ Color
        -> IO ()
           
-- |Draw a vertical line, with alpha blending           
lineVBlend :: Surface -> Int -> Int -> Int -> Pixel -> Word8 -> IO ()

-- |Draw a line of arbitrary direction
line :: Surface -- ^ SDL destination surface
       -> Int -- ^ Start X
       -> Int -- ^ Start Y
       -> Int -- ^ End X
       -> Int -- ^ End Y
       -> Pixel -- ^ Color
       -> IO ()

-- |Draw a line with alpha blending
lineBlend :: Surface -> Int -> Int -> Int -> Int -> Pixel -> Word8 -> IO ()
-- |Draw a line with a gradient fill (between two colors)
lineFade :: Surface -> Int -> Int -> Int -> Int -> Pixel -> Pixel -> IO ()
-- |Draw a line with a gradient fill and alpha gradient
lineFadeBlend :: Surface -> Int -> Int -> Int -> Int -> Pixel -> Word8 -> Pixel -> Word8 -> IO ()
-- |Draw a rectangle
rect :: Surface -- ^ SDL destination surface
        -> Int -- ^ X1
        -> Int -- ^ Y1
        -> Int -- ^ X2
        -> Int -- ^ Y2
        -> Pixel -- ^ Color
        -> IO ()
-- |Draw a rectangle, with alpha blending          
rectBlend :: Surface -> Int -> Int -> Int -> Int -> Pixel -> Word8 -> IO ()
-- |Draw a filled rectangle
rectFilled :: Surface -> Int -> Int -> Int -> Int -> Pixel -> IO ()
-- |Draw a filled rectangle with alpha blending
rectFilledBlend :: Surface -> Int -> Int -> Int -> Int -> Pixel -> Word8 -> IO ()
-- |Draw a rounded rectangle
rectRound :: Surface -- ^ SDL destination surface
           -> Int -- ^ X1
           -> Int -- ^ Y1
           -> Int -- ^ X2
           -> Int -- ^ Y2
           -> Float -- ^ Corner radius
           -> Pixel -- ^ Color
           -> IO ()
-- |Draw a rounded rectangle with alpha blending               
rectRoundBlend :: Surface -> Int -> Int -> Int -> Int -> Float -> Pixel -> Word8 -> IO ()
-- |Draw a filled rounded rectangle
rectRoundFilled :: Surface -> Int -> Int -> Int -> Int -> Float -> Pixel -> IO ()
-- |Draw a filled rounded rectangle with alpha blending
rectRoundFilledBlend :: Surface -> Int -> Int -> Int -> Int -> Float -> Pixel -> Word8 -> IO ()
-- |Draw an arc
arc :: Surface -- ^ SDL destination surface
        ->    Int -- ^ X (center)
        ->    Int -- ^ Y (center)
        ->  Float -- ^ Radius
        ->  Float -- ^ Start angle (in degrees or radians depending on Control state)
        ->  Float -- ^ End angle
        ->  Pixel 
        ->     IO ()
-- |Draw an arc, with alpha blending            
arcBlend :: Surface -> Int -> Int -> Float -> Float -> Float -> Pixel -> Word8 -> IO ()
-- |Draw a filled arc
arcFilled :: Surface -> Int -> Int -> Float -> Float -> Float -> Pixel -> IO ()
-- |Draw a filled arc, with alpha blending
arcFilledBlend :: Surface -> Int -> Int -> Float -> Float -> Float -> Pixel -> Word8 -> IO ()
-- |Draw a circle
circle :: Surface  -- ^ SDL destination surface
         ->     Int  -- ^ X (center)
         ->     Int  -- ^ Y (center)
         ->   Float  -- ^ Radius
         ->   Pixel  -- ^ Color
         ->      IO ()

-- |Draw a circle, with alpha blending               
circleBlend :: Surface -> Int -> Int -> Float -> Pixel -> Word8 -> IO ()
-- |Draw a filled circle
circleFilled :: Surface -> Int -> Int -> Float -> Pixel -> IO ()
-- |Draw a filled circle, with alpha blending
circleFilledBlend :: Surface -> Int -> Int -> Float -> Pixel -> Word8 -> IO ()
-- |Draw an ellipse
ellipse :: Surface -- ^ SDL destination surface
           ->     Int -- ^ X
           ->     Int -- ^ Y
           ->   Float -- ^ X Radius
           ->   Float -- ^ Y Radius
           ->   Pixel -- ^ Color
           ->      IO ()
-- |Draw an ellipse, with alpha blending               
ellipseBlend :: Surface -> Int -> Int -> Float -> Float -> Pixel -> Word8 -> IO ()
-- |Draw a filled ellipse
ellipseFilled :: Surface -> Int -> Int -> Float -> Float -> Pixel -> IO ()
-- |Draw a filled ellipse, with alpha blending
ellipseFilledBlend :: Surface -> Int -> Int -> Float -> Float -> Pixel -> Word8 -> IO ()
-- |Draw an ellipse at the given rotation angle
ellipseArb :: Surface -- ^ SDL destination surface
               ->    Int -- ^ X
               ->    Int -- ^ Y
               ->  Float -- ^ X radius
               ->  Float -- ^ Y radius
               ->  Float -- ^ Rotation angle (degrees/radians specified by Control State)
               ->  Pixel -- ^ Color
               ->  IO ()
-- |Draw an arbitrary ellipse, with alpha blending                
ellipseBlendArb :: Surface -> Int -> Int -> Float -> Float -> Float -> Pixel -> Word8 -> IO ()
-- |Draw an arbitrary filled ellipse
ellipseFilledArb :: Surface -> Int -> Int -> Float -> Float -> Float -> Pixel -> IO ()
-- |Draw an arbitrary filled ellipse, with alpha blending
ellipseFilledBlendArb :: Surface -> Int -> Int -> Float -> Float -> Float -> Pixel -> Word8 -> IO ()
-- |Draw a bezier curve
bezier :: Surface  -- ^ SDL destination surface
            ->  Int  -- ^ Start X
            ->  Int  -- ^ Start Y
            ->  Int  -- ^ First Control Point X
            ->  Int  -- ^ First Control Point Y
            ->  Int  -- ^ Second Control Point X
            ->  Int  -- ^ Second Control Point Y
            ->  Int  -- ^ End X
            ->  Int  -- ^ End Y
            ->Word8  -- ^ Quality (number of intermediate points). A quality of 4-7 is normal.
            ->Pixel  -- ^ Color
            ->IO ()      
-- |Draw a bezier curve with alpha blending            
bezierBlend :: Surface -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Word8 -> Pixel -> Word8 -> IO ()

withSurface :: Surface -> (Ptr SurfaceStruct -> IO a) -> IO a
withSurface = withForeignPtr

withInteger :: Int -> (Int16 -> a) -> a
withInteger i f = f (fromIntegral i)

withPixelVl :: Pixel -> (Word32 -> a) -> a
withPixelVl (Pixel v) f = f v

withWordVal :: Word8 -> (Word8 -> a) -> a
withWordVal = flip ($)

withDoubleP :: Float -> (Float -> a) -> a
withDoubleP = flip ($)

pixel                a a1 a2 a3 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withPixelVl a3 $ \a3' -> spgPixel a' a1' a2' a3'
pixelBlend           a a1 a2 a3 a4 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withPixelVl a3 $ \a3' -> withWordVal a4 $ \a4' -> spgPixelBlend a' a1' a2' a3' a4'
lineH                a a1 a2 a3 a4 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withPixelVl a4 $ \a4' -> spgLineH a' a1' a2' a3' a4'
lineV                a a1 a2 a3 a4 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withPixelVl a4 $ \a4' -> spgLineV a' a1' a2' a3' a4'
circle               a a1 a2 a3 a4 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withPixelVl a4 $ \a4' -> spgCircle a' a1' a2' a3' a4'
circleFilled         a a1 a2 a3 a4 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withPixelVl a4 $ \a4' -> spgCircleFilled a' a1' a2' a3' a4'
circleBlend          a a1 a2 a3 a4 a5 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withPixelVl a4 $ \a4' -> withWordVal a5 $ \a5' -> spgCircleBlend a' a1' a2' a3' a4' a5'
ellipseFilled        a a1 a2 a3 a4 a5 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withDoubleP a4 $ \a4' -> withPixelVl a5 $ \a5' -> spgEllipseFilled a' a1' a2' a3' a4' a5'
circleFilledBlend    a a1 a2 a3 a4 a5 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withPixelVl a4 $ \a4' -> withWordVal a5 $ \a5' -> spgCircleFilledBlend a' a1' a2' a3' a4' a5'
ellipse              a a1 a2 a3 a4 a5 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withDoubleP a4 $ \a4' -> withPixelVl a5 $ \a5' -> spgEllipse a' a1' a2' a3' a4' a5'
lineHBlend           a a1 a2 a3 a4 a5 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withPixelVl a4 $ \a4' -> withWordVal a5 $ \a5' -> spgLineHBlend a' a1' a2' a3' a4' a5'
lineVBlend           a a1 a2 a3 a4 a5 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withPixelVl a4 $ \a4' -> withWordVal a5 $ \a5' -> spgLineVBlend a' a1' a2' a3' a4' a5'
line                 a a1 a2 a3 a4 a5 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withPixelVl a5 $ \a5' -> spgLine a' a1' a2' a3' a4' a5'
lineHFade            a a1 a2 a3 a4 a5 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withPixelVl a4 $ \a4' -> withPixelVl a5 $ \a5' -> spgLineHFade a' a1' a2' a3' a4' a5'
rect                 a a1 a2 a3 a4 a5 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withPixelVl a5 $ \a5' -> spgRect a' a1' a2' a3' a4' a5'
rectFilled           a a1 a2 a3 a4 a5 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withPixelVl a5 $ \a5' -> spgRectFilled a' a1' a2' a3' a4' a5'
lineBlend            a a1 a2 a3 a4 a5 a6 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withPixelVl a5 $ \a5' -> withWordVal a6 $ \a6' -> spgLineBlend a' a1' a2' a3' a4' a5' a6'
lineFade             a a1 a2 a3 a4 a5 a6 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withPixelVl a5 $ \a5' -> withPixelVl a6 $ \a6' -> spgLineFade a' a1' a2' a3'  a4' a5' a6'
rectBlend            a a1 a2 a3 a4 a5 a6 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withPixelVl a5 $ \a5' -> withWordVal a6 $ \a6' -> spgRectBlend a' a1' a2' a3' a4' a5' a6'
rectFilledBlend      a a1 a2 a3 a4 a5 a6 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withPixelVl a5 $ \a5' -> withWordVal a6 $ \a6' -> spgRectFilledBlend a' a1' a2' a3' a4' a5' a6'
rectRound            a a1 a2 a3 a4 a5 a6 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withDoubleP a5 $ \a5' -> withPixelVl a6 $ \a6' -> spgRectRound a' a1' a2' a3' a4' a5' a6'
rectRoundFilled      a a1 a2 a3 a4 a5 a6 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withDoubleP a5 $ \a5' -> withPixelVl a6 $ \a6' -> spgRectRoundFilled a' a1' a2' a3' a4' a5' a6'
arc                  a a1 a2 a3 a4 a5 a6 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withDoubleP a4 $ \a4' -> withDoubleP a5 $ \a5' -> withPixelVl a6 $ \a6' -> spgArc a' a1' a2' a3' a4' a5' a6'
arcFilled            a a1 a2 a3 a4 a5 a6 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withDoubleP a4 $ \a4' -> withDoubleP a5 $ \a5' -> withPixelVl a6 $ \a6' -> spgArcFilled a' a1' a2' a3' a4' a5' a6'
ellipseBlend         a a1 a2 a3 a4 a5 a6 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withDoubleP a4 $ \a4' -> withPixelVl a5 $ \a5' -> withWordVal a6 $ \a6' -> spgEllipseBlend a' a1' a2' a3' a4' a5' a6'
ellipseFilledBlend   a a1 a2 a3 a4 a5 a6 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withDoubleP a4 $ \a4' -> withPixelVl a5 $ \a5' -> withWordVal a6 $ \a6' -> spgEllipseFilledBlend a' a1' a2' a3' a4' a5' a6'
ellipseArb           a a1 a2 a3 a4 a5 a6 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withDoubleP a4 $ \a4' -> withDoubleP a5 $ \a5' -> withPixelVl a6 $ \a6' -> spgEllipseArb a' a1' a2' a3' a4' a5' a6'
ellipseFilledArb     a a1 a2 a3 a4 a5 a6 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withDoubleP a4 $ \a4' -> withDoubleP a5 $ \a5' -> withPixelVl a6 $ \a6' -> spgEllipseFilledArb a' a1' a2' a3' a4' a5' a6'
rectRoundBlend       a a1 a2 a3 a4 a5 a6 a7 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withDoubleP a5 $ \a5' -> withPixelVl a6 $ \a6' -> withWordVal a7 $ \a7' -> spgRectRoundBlend a' a1' a2' a3'  a4' a5' a6' a7'
rectRoundFilledBlend a a1 a2 a3 a4 a5 a6 a7 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withDoubleP a5 $ \a5' -> withPixelVl a6 $ \a6' -> withWordVal a7 $ \a7' -> spgRectRoundFilledBlend a' a1' a2' a3' a4' a5' a6' a7'
arcBlend             a a1 a2 a3 a4 a5 a6 a7 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withDoubleP a4 $ \a4' -> withDoubleP a5 $ \a5' -> withPixelVl a6 $ \a6' -> withWordVal a7 $ \a7' -> spgArcBlend a' a1' a2' a3' a4' a5' a6' a7'
arcFilledBlend       a a1 a2 a3 a4 a5 a6 a7 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withDoubleP a4 $ \a4' -> withDoubleP a5 $ \a5' -> withPixelVl a6 $ \a6' -> withWordVal a7 $ \a7' -> spgArcFilledBlend a' a1' a2' a3' a4' a5' a6' a7'
ellipseBlendArb      a a1 a2 a3 a4 a5 a6 a7  = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withDoubleP a4 $ \a4' -> withDoubleP a5 $ \a5' -> withPixelVl a6 $ \a6' -> withWordVal a7 $ \a7' -> spgEllipseBlendArb a' a1' a2' a3' a4' a5' a6' a7'
ellipseFilledBlendArb a a1 a2 a3 a4 a5 a6 a7 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withDoubleP a3 $ \a3' -> withDoubleP a4 $ \a4' -> withDoubleP a5 $ \a5' -> withPixelVl a6 $ \a6' -> withWordVal a7 $ \a7' -> spgEllipseFilledBlendArb a' a1' a2' a3' a4' a5' a6' a7'
lineFadeBlend         a a1 a2 a3 a4 a5 a6 a7 a8 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withPixelVl a5 $ \a5' -> withWordVal a6 $ \a6' -> withPixelVl a7 $ \a7' -> withWordVal a8 $ \a8' -> spgLineFadeBlend a' a1' a2' a3' a4' a5' a6' a7' a8'
lineHTex              a a1 a2 a3 a4 a5 a6 a7 a8 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withSurface a4 $ \a4' -> withInteger a5 $ \a5' -> withInteger a6 $ \a6' -> withInteger a7 $ \a7' -> withInteger a8 $ \a8' -> spgLineHTex a' a1' a2' a3' a4' a5' a6' a7' a8'
bezier                a a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withInteger a5 $ \a5' -> withInteger a6 $ \a6' -> withInteger a7 $ \a7' -> withInteger a8 $ \a8' -> withWordVal a9 $ \a9' -> withPixelVl a10 $ \a10' -> spgBezier a' a1' a2' a3' a4' a5' a6' a7' a8' a9' a10'
bezierBlend           a a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = withSurface a $ \a' -> withInteger a1 $ \a1' -> withInteger a2 $ \a2' -> withInteger a3 $ \a3' -> withInteger a4 $ \a4' -> withInteger a5 $ \a5' -> withInteger a6 $ \a6' -> withInteger a7 $ \a7' -> withInteger a8 $ \a8' -> withWordVal a9 $ \a9' -> withPixelVl a10 $ \a10' -> withWordVal a11 $ \a11' -> spgBezierBlend a' a1' a2' a3' a4' a5' a6' a7' a8' a9' a10' a11'



