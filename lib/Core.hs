{-# LANGUAGE FlexibleContexts #-}

module Core
  ( recursiveImageRelative
  , recursiveImage
  )  where

import qualified Graphics.Image as I
import qualified Graphics.Image.Processing as P

import Numeric.Extra (intToDouble)

type Point = (Double, Double)

recursiveImageRelative :: (I.Array arr I.RGBA Double)
               => Point          -- ^ The upper left corner 
               -> Point          -- ^ The lower right corner
               -> Int                -- ^ The number of recusive steps
               -> I.Image arr I.RGBA Double     -- ^ The source image
               -> I.Image arr I.RGBA Double     -- ^ The resulting image
recursiveImageRelative (rx1,ry1) (rx2,ry2) n i =
  recursiveImage (x1,y1) (x2,y2) n i
  where [x1,x2] = toAbsolute I.cols <$> [rx1,rx2]
        [y1,y2] = toAbsolute I.rows <$> [ry1,ry2]
        toAbsolute dim = round <$> (* (intToDouble $ dim i))
        
recursiveImage :: (I.Array arr I.RGBA Double)
               => (Int, Int)          -- ^ The upper left corner 
               -> (Int, Int)          -- ^ The lower right corner
               -> Int                -- ^ The number of recusive steps
               -> I.Image arr I.RGBA Double     -- ^ The source image
               -> I.Image arr I.RGBA Double     -- ^ The resulting image
recursiveImage ul lr n i = i'
  where i' = (iterate (next i) i) !! n
        next i j = scaledEmbed ul lr j i
  
-- |
scaledEmbed :: (I.Array arr I.RGBA Double)
               => (Int, Int)          -- ^ The upper left corner 
               -> (Int, Int)          -- ^ The lower right corner
               -> I.Image arr I.RGBA Double     -- ^ Image to be embedded
               -> I.Image arr I.RGBA Double     -- ^ Image to be embedded in
               -> I.Image arr I.RGBA Double     -- ^ The resulting image
scaledEmbed ul lr i1 i2 = i'
  where scaledDown = scaleDown ul lr i1
        embeddedI = I.zipWith overlay scaledDown i2
        i' = embeddedI

scaleDown ul@(rx1,ry1) lr@(rx2,ry2) i = scaledDown
  where [sx,sy] = zipWith (-) [rx2,ry2] [rx1,ry1]
        smallI = P.resize P.Bilinear P.Edge (sy,sx) i
        extendedI = P.canvasSize (P.Fill 0) (I.dims i) smallI
        translatedI = P.translate (P.Fill 0) (ry1,rx1) extendedI
        scaledDown = translatedI

overlay :: I.Pixel I.RGBA Double  -- ^ foreground
        -> I.Pixel I.RGBA Double  -- ^ background
        -> I.Pixel I.RGBA Double
overlay (I.PixelRGBA _ _ _ 0) px = px
overlay px _ = px

-- |Alpha blend two images
-- https://en.wikipedia.org/wiki/Alpha_compositing#Alpha_blending
blend :: I.Pixel I.RGBA Double  -- ^ foreground
      -> I.Pixel I.RGBA Double  -- ^ background
      -> I.Pixel I.RGBA Double
blend (I.PixelRGBA rf gf bf af) (I.PixelRGBA rb gb bb ab) =
  let a = af + ab * (1 - af)
  in if a <= 0.001 then I.PixelRGBA 0 0 0 0
  else let [r,g,b] = (/a) <$> zipWith (+) 
                     ((*af) <$> [rf,gf,bf])
                     ((*(ab*(1-af))) <$> [rb,gb,bb])
       in I.PixelRGBA r g b a
