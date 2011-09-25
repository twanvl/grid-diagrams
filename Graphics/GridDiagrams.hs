{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}

module Graphics.GridDiagrams
    ( -- * Colors
      Color (..)
    , rgba, rgb, hsla, hsl
    , transparent, gray, white, black, red, green, blue, cyan, magenta, yellow
    , alpha, over
    
    , Render, Diagram, RenderOpts(..), defaultRenderOpts
    , getGridSize
    
    , renderToFile, renderToFile'

	-- * Lifted Cairo operations
    , liftCairo
    , setLineWidth, setColor, selectFontFace, setFontSize, setLineCap, setLineJoin
    , FontSlant(..), FontWeight(..), LineCap(..), LineJoin(..)
    , fill, stroke, fillPreserve, fillStroke, extend
    , saved, translated
    , circle, rectangle, line, showText, showTextCentered
    
    , Drawable(..)
    , drawRowsWith, drawColsWith, drawCellsWith, drawCells
    , drawRowBorders, drawColBorders, drawBordersWith, drawBorders
    , drawGrid
    -- * Borders around \/ between cells
    , Orientation(..), drawBorder, borderNone, borderSimple, borderIfSame
    ) where

import Data.List
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo (FontSlant(..), FontWeight(..), LineCap(..), LineJoin(..))

-------------------------------------------------------------------------------
-- Working with colors
-------------------------------------------------------------------------------

-- | Colors: red,green,blue,alpha, all in the range [0..1]
data Color = RGBA !Double !Double !Double !Double
    deriving (Eq,Ord,Read,Show)

-- | Create a color from hue,saturation,ligthness,alpha.
-- All parameters are in range [0..1]
hsla :: Double -> Double -> Double -> Double -> Color
hsla h s l a
    | h' < 1    = RGBA (m+c) (m+x) (m+0) (a)
    | h' < 2    = RGBA (m+x) (m+c) (m+0) (a)
    | h' < 3    = RGBA (m+0) (m+c) (m+x) (a)
    | h' < 4    = RGBA (m+0) (m+x) (m+c) (a)
    | h' < 5    = RGBA (m+x) (m+0) (m+c) (a)
    | otherwise = RGBA (m+c) (m+0) (m+x) (a)
  where
    h' = (h `fmod` 1) * 6
    c = (1 - abs (2*l-1)) * s
    x = c * (1 - abs (h' `fmod` 2 - 1))
    m = l - c*0.5

fmod :: Double -> Double -> Double
a `fmod` b = snd (properFraction (a/b+1000)) * b

-- | Create a color from hue,saturation,ligthness
hsl :: Double -> Double -> Double -> Color
hsl h s l = hsla h s l 1

-- | Create a color from red,green,blue,alpha
rgba :: Double -> Double -> Double -> Double -> Color
rgba = RGBA

-- | Create a color from red,green,blue
rgb :: Double -> Double -> Double -> Color
rgb r g b = RGBA r g b 1

gray :: Double -> Color
gray x = rgb x x x

transparent, white, black, red, green, blue, cyan, magenta, yellow :: Color
transparent = RGBA 0 0 0 0
white   = rgb 1 1 1
black   = rgb 0 0 0
red     = rgb 1 0 0
green   = rgb 0 1 0
blue    = rgb 0 0 1
cyan    = rgb 0 1 1
magenta = rgb 1 0 1
yellow  = rgb 1 1 0

alpha :: Double -> Color -> Color
alpha f (RGBA r g b a) = RGBA r g b (f*a)

-- | Overlay two colors
over :: Color -> Color -> Color
RGBA r g b a `over` RGBA r' g' b' a' = RGBA (x2 r r') (x2 g g') (x2 b b') a2
  where
    a2 = a + (1-a)*a'
    x2 x x' = ( (x*a) + (1-a)*(x'*a') ) / a2

{-
-- color arithmathic, the stupid way
instance Num Color where
    fromInteger a  = (fromInteger a,fromInteger a,fromInteger a,fromInteger a)
    RGB r g b a + RGB r' g' b' a' = RGB (r+r') (g+g') (b+b') (a'+a')
    RGB r g b a * RGB r' g' b' a' = (a*a',b*b',c*c',d*d')
    RGB r g b a - RGB r' g' b' a' = (a-a',b-b',c-c',d-d')
    abs (RGB r g b a)             = (abs a,abs b,abs c,abs d)
    signum (RGB r g b a)          = (signum a,signum b,signum c,signum d)
instance Fractional Color where
    fromRational a  = RGBA (fromRational a) (fromRational a) (fromRational a) (fromRational a)
    (a,b,c,d) / (a',b',c',d') = (a/a',b/b',c/c',d/d')
-}

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Extents = Extents { minX,minY,maxX,maxY :: !Double }
    deriving (Show)

instance Monoid Extents where
    mempty = Extents (1/0) (1/0) (-1/0) (-1/0)
    mappend a b = Extents (minX a `min` minX b) (minY a `min` minY b) (maxX a `max` maxX b) (maxY a `max` maxY b)

padExtents :: Double -> Extents -> Extents
padExtents r (Extents a b c d) = Extents (a-r) (b-r) (c+r) (d+r)

data RenderOpts = RenderOpts
    { gridSize    :: Double
    , scale       :: Double
    , fillPadding :: Double
    , bgColor     :: Color
    }

defaultRenderOpts :: RenderOpts
defaultRenderOpts = RenderOpts 24 1 1.5 white

-------------------------------------------------------------------------------
-- Rendering monad
-------------------------------------------------------------------------------

data Render_ a = R_ { rExtent :: !(WriterT Extents C.Render a), rDraw :: !(C.Render a) }
newtype Render a = R (ReaderT RenderOpts Render_ a)
    deriving (Functor, Applicative, Monad, MonadIO)

type Diagram = Render ()

instance Functor Render_ where
    fmap f (R_ x y) = R_ (fmap f x) (fmap f y)
instance Applicative Render_ where
    pure = return
    (<*>) = ap
instance Monad Render_ where
    return x = R_ (return x) (return x)
    R_ x y >>= f = R_ (x >>= rExtent . f) (y >>= rDraw . f)
instance MonadIO Render_ where
    liftIO x = R_ (liftIO x) (liftIO x)

liftCairo :: C.Render a -> Render a
liftCairo x = liftSep (lift x) x

liftSep :: WriterT Extents C.Render a -> C.Render a -> Render a
liftSep a b = R $ lift (R_ a b)


getGridSize :: Render Double
getGridSize = R $ asks gridSize

-------------------------------------------------------------------------------
-- Lifted Cairo operations
-------------------------------------------------------------------------------

-- lifting Cairo ops

setLineWidth :: Double -> Diagram
setLineWidth = liftCairo . C.setLineWidth

setLineCap :: LineCap -> Diagram
setLineCap = liftCairo . C.setLineCap

setLineJoin :: LineJoin -> Diagram
setLineJoin = liftCairo . C.setLineJoin

setFontSize :: Double -> Diagram
setFontSize = liftCairo . C.setFontSize

selectFontFace :: String -> FontSlant -> FontWeight -> Diagram
selectFontFace x y z = liftCairo $ C.selectFontFace x y z

setColor :: Color -> Diagram
setColor = liftCairo . cSetColor
cSetColor (RGBA r g b a) = C.setSourceRGBA r g b a

stroke,fill,fillPreserve,extend :: Diagram
stroke       = liftSep (updateExtents >> lift C.newPath) C.stroke
fill         = liftSep (updateExtents >> lift C.newPath) C.fill
fillPreserve = liftSep updateExtents C.fillPreserve
extend       = liftSep (updateExtents >> lift C.newPath) C.newPath
updateExtents = do
    (x1,y1,x2,y2) <- lift $ do
        lw <- C.getLineWidth
        if lw < 0.005 then C.fillExtents else C.strokeExtents
    (x1',y1') <- lift $ C.userToDevice x1 y1
    (x2',y2') <- lift $ C.userToDevice x2 y2
    tell (Extents x1' y1' x2' y2')

fillStroke :: Color -> Color -> Diagram
fillStroke f s = setColor f >> fillPreserve >> setColor s >> stroke

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

saved :: Diagram -> Diagram
saved r = liftCairo C.save >> r >> liftCairo C.restore

translated :: Double -> Double -> Diagram -> Diagram
translated x y r = do
    g <- getGridSize
    saved $ liftCairo (C.translate (x*g) (y*g)) >> r

-------------------------------------------------------------------------------
-- Drawing primitives
-------------------------------------------------------------------------------

circle :: Double -> Double -> Double -> Diagram
circle x y r = do
    g <- getGridSize
    liftCairo $ C.arc (x*g) (y*g) (r*g) 0 (2*pi)

rectangle :: Double -> Double -> Double -> Double -> Diagram
rectangle x y w h = do
    g <- getGridSize
    liftCairo $ C.rectangle (x*g) (y*g) (w*g) (h*g)

line :: Double -> Double -> Double -> Double -> Diagram
line x y x' y' = do
    g <- getGridSize
    saved $ liftCairo $ C.moveTo (x*g) (y*g) >> C.lineTo (x'*g) (y'*g)

showText :: String -> Render ()
showText str = liftSep (toExtents =<< lift (C.textExtents str)) (C.showText str)
  where
    toExtents e = do
        (x,y) <- lift $ C.userToDevice 0 0
        (w,h) <- lift $ C.userToDeviceDistance (C.textExtentsWidth e) (C.textExtentsHeight e)
        let e' = Extents x (y-h) (x+w) y
        tell $ e'

showTextCentered :: String -> Render ()
showTextCentered str = saved $ do
    ext <- liftCairo $ C.textExtents str
    liftCairo $ C.translate (-0.5 * C.textExtentsXadvance ext) (0.5 * C.textExtentsHeight ext)
    showText str

-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------

renderToFile' :: FilePath -> RenderOpts -> Diagram -> IO ()
renderToFile' fn
    = runRender $ \w h r -> case ext of
        "png" -> C.withImageSurface C.FormatARGB32 (round w) (round h) $
                   \s -> C.renderWith s r >> C.surfaceWriteToPNG s fn
        "pdf" -> C.withPDFSurface fn w h (flip C.renderWith r)
        "ps"  -> C.withPSSurface  fn w h (flip C.renderWith r)
        "svg" -> C.withSVGSurface fn w h (flip C.renderWith r)
  where
    ext = reverse . takeWhile (/='.') . reverse $ fn

renderToFile :: FilePath -> Diagram -> IO ()
renderToFile fn = renderToFile' fn defaultRenderOpts

getExtents :: RenderOpts -> Render a -> IO Extents
getExtents opts (R r)
    = C.withImageSurface C.FormatARGB32 0 0 $ \s ->
      C.renderWith s $ execWriterT $ rExtent $ runReaderT r opts

runRender :: (Double -> Double -> C.Render a -> IO b) -> RenderOpts -> Render a -> IO b
runRender f opts (R r) = do
    ext <- getExtents opts (R r)
    let w = max 0 (maxX ext - minX ext)
    let h = max 0 (maxY ext - minY ext)
    f w h $ do
        cSetColor (bgColor opts)
        C.rectangle 0 0 w h
        C.fill
        C.translate (-minX ext) (-minY ext)
        rDraw $ runReaderT r opts

-------------------------------------------------------------------------------
-- Grid cell drawing
-------------------------------------------------------------------------------

drawRowsWith :: (a -> Diagram) -> [a] -> Diagram
drawRowsWith f = sequence_ . zipWith (\y a -> translated 0 y $ f a) [0..]

drawColsWith :: (a -> Diagram) -> [a] -> Diagram
drawColsWith f = sequence_ . zipWith (\x a -> translated x 0 $ f a) [0..]

drawCellsWith :: (a -> Diagram) -> [[a]] -> Diagram
drawCellsWith = drawRowsWith . drawColsWith

drawCells :: Drawable a => [[a]] -> Diagram
drawCells = drawCellsWith draw

-------------------------------------------------------------------------------
-- Grid border drawing
-------------------------------------------------------------------------------

data Orientation = H | V

type BorderDrawer a = Orientation -> Maybe a -> Maybe a -> Diagram

borderNone   :: BorderDrawer a
borderSimple :: Color -> Double -> BorderDrawer a
borderIfSame :: Eq a => Color -> Double -> Color -> Double -> BorderDrawer a

borderNone _ _ _ = return ()
borderSimple c w o _ _ = drawBorder o c w
borderIfSame c1 w1 c2 w2 o a b = if (a == b) then drawBorder o c1 w1 else drawBorder o c2 w2

drawBorder :: Orientation -> Color -> Double -> Diagram
drawBorder H c w = setColor c >> setLineWidth w >> line 0 0 0 1 >> stroke
drawBorder V c w = setColor c >> setLineWidth w >> line 0 0 1 0 >> stroke


drawRowBorders :: (Maybe a -> Maybe a -> Diagram) -> [a] -> Diagram
drawRowBorders f xs = sequence_ $ zipWith3 fAt [0..] xs' (tail xs')
  where
    xs' = [Nothing] ++ map Just xs ++ [Nothing]
    fAt x a b = translated x 0 $ f a b

drawColBorders :: (Maybe a -> Maybe a -> Diagram) -> [a] -> Diagram
drawColBorders f xs = sequence_ $ zipWith3 fAt [0..] xs' (tail xs')
  where
    xs' = [Nothing] ++ map Just xs ++ [Nothing]
    fAt y a b = translated 0 y $ f a b

drawBordersWith :: (Orientation -> Maybe a -> Maybe a -> Diagram) -> [[a]] -> Diagram
drawBordersWith f xs = drawRowsWith (drawRowBorders (f H)) xs 
                    >> drawColsWith (drawColBorders (f V)) (transpose xs)

drawBorders :: [[a]] -> Diagram
drawBorders = drawBordersWith (borderSimple black 1)

drawGrid :: Drawable a => [[a]] -> Diagram
drawGrid xs = setLineWidth 3 >> drawCells xs >> drawBorders xs

-------------------------------------------------------------------------------
-- Drawable class
-------------------------------------------------------------------------------

class Drawable a where
    draw :: a -> Diagram

instance Drawable Color where
    draw c = setColor c >> rectangle 0 0 1 1 >> fill

instance Drawable String where
    draw str = do
        g <- getGridSize
        selectFontFace "Ubuntu" C.FontSlantNormal C.FontWeightNormal
        setFontSize (g * 0.75)
        setColor black
        translated 0.5 0.5 $ showTextCentered str

instance Drawable a => Drawable [[a]] where
    draw = drawGrid

instance (Drawable a, Drawable b) => Drawable (a,b) where
    draw (a,b) = draw a >> draw b

instance (Drawable a, Drawable b) => Drawable (Either a b) where
    draw (Left  a) = draw a
    draw (Right b) = draw b

instance (Drawable a) => Drawable (Maybe a) where
    draw (Just a) = draw a
    draw Nothing  = return ()

-------------------------------------------------------------------------------
-- Grid utilities
-------------------------------------------------------------------------------
{-
drawBorder white 3

type Colored = (Color -> Diagram) -> Diagram

row, column :: [a] -> [[a]]
row = return
column = map return

(|||) :: [[a]] -> [[b]] -> [[Either a b]]
(|||) = hcatWith Left Right

(===) :: [[a]] -> [[b]] -> [[Either a b]]
(===) = vcatWith Left Right

(===) = (++)
(===) :: a -> b -> Diagram
(===) = (++)

grids :: 

 a ||| b
===
 c

[[()]] ||| sums


Is '-' => False
Eq     => True

-}
