{-# LANGUAGE FlexibleContexts #-}

import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified Graphics.Image as I
import qualified Graphics.Image.Processing as P

import Data.IORef
import Numeric.Extra (intToDouble)

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    startGUI defaultConfig setup

htmlCanvasSize = 400

setup :: Window -> UI ()
setup window = do
    return window # set title "Canvas - Examples"

    canvas <- UI.canvas
        # set UI.height htmlCanvasSize
        # set UI.width  htmlCanvasSize
        # set style [("border", "solid black 1px"), ("background", "#eee")]

    clear     <- UI.button #+ [string "Clear the canvas."]

    url <- UI.loadFile "image/jpg" "static/image.jpg"
    img <- UI.img # set UI.src url

    cat <- liftIO $ I.readImageRGB I.VU "static/image.jpg"

    let drawImage :: UI ()
        drawImage = do
          canvas # UI.drawImage img (0,0)


    -- selecting <- liftIO $ newIORef False
    -- -- startPoint :: IORef (Double,Double)
    -- startPoint <- liftIO $ newIORef (0 :: Double,0 :: Double)

    let eStart = UI.mousedown canvas
    bStart <- stepper (0,0) eStart
    let bSetStart = (\s -> \p -> (s,p)) <$> bStart
        eEnd = unionWith (\a b -> b) (const () <$> UI.mouseup canvas) (UI.leave canvas)
        eMove = UI.mousemove canvas
    bSelecting <- stepper False $ unionWith (\a b -> b)
                     (const True <$> eStart)
                     (const False <$> eEnd)
    let eSelect :: Event ((Int,Int),(Int,Int))
        eSelect = whenE bSelecting $ apply bSetStart eMove

    onEvent eEnd $ const $ do
      canvas # UI.clearCanvas
      drawImage
      
    onEvent eSelect $ \((sx,sy),(x,y)) -> do
      let [sx', sy', x', y'] = intToDouble <$> [sx,sy,x,y]
      canvas # UI.clearCanvas
      drawImage
      canvas # UI.beginPath
      canvas # UI.moveTo (sx',sy')
      canvas # UI.lineTo (sx',y')
      canvas # UI.lineTo (x',y')
      canvas # UI.lineTo (x',sy')
      canvas # UI.closePath
      canvas # UI.stroke

      
    getBody window #+
        [ column [element canvas]
        , element clear
        ]

    on UI.click clear $ const $
        canvas # UI.clearCanvas
    
    return ()
    -- on UI.mousedown canvas $ \(x,y) -> do
    --   liftIO $ modifyIORef selecting $ const True
    --   liftIO $ modifyIORef startPoint $ const (intToDouble x,intToDouble y)

    -- on UI.mouseup canvas $ \(x,y) -> do
    --   liftIO $ modifyIORef selecting $ const False
    --   canvas # UI.clearCanvas
    -- on UI.leave canvas $ const $ do
    --   liftIO $ modifyIORef selecting $ const False
    --   canvas # UI.clearCanvas
      
    -- on UI.mousemove canvas $ \(x,y) -> do
    --   doSelect <- liftIO $ readIORef selecting
      
    --   let dx = intToDouble x
    --       dy = intToDouble y
    --   when doSelect $ do
    --     canvas # UI.clearCanvas
    --     canvas # UI.beginPath
    --     sp@(sx,sy) <- liftIO $ readIORef startPoint
    --     canvas # UI.moveTo sp
    --     canvas # UI.lineTo (sx,dy)
    --     canvas # UI.lineTo (dx,dy)
    --     canvas # UI.lineTo (dx,sy)
    --     canvas # UI.closePath
    --     canvas # UI.stroke

type Point = (Double,Double)

-- |Embed an image in itself
embed :: (I.Array arr I.RGBA Double)
      => (Int,Int)          -- ^ The upper left corner 
      -> (Int,Int)          -- ^ The lower right corner
      -> I.Image arr I.RGBA Double     -- ^ The source image
      -> I.Image arr I.RGBA Double     -- ^ The resulting image
embed ul@(x1,y1) lr@(x2,y2) i = i'
  where d = (abs $ x2-x1,abs $ y2-y1)
        smallI = P.resize P.Bilinear P.Edge d i
        extendedI = P.canvasSize (P.Fill 0) (I.dims i) smallI
        translatedI = P.translate (P.Fill 0) ul extendedI
        embeddedI = I.zipWith (addpx) i translatedI
        -- ignore alpha pixels
        addpx :: I.Pixel I.RGBA Double -> I.Pixel I.RGBA Double -> I.Pixel I.RGBA Double
        addpx px (I.PixelRGBA _ _ _ 0) = px
        addpx _ px = px
        i' = embeddedI
