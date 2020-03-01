import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Data.IORef
import Numeric.Extra (intToDouble)

import qualified Graphics.Image as I

import System.Directory (getCurrentDirectory)

import Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    startGUI (defaultConfig { jsStatic = Just "static/", jsCustomHTML = Just "index.html", jsCallBufferMode = NoBuffering }) setup

htmlCanvasSize = 400

setup :: Window -> UI ()
setup window = do
    return window # set title "Canvas - Examples"

    canvas <- UI.canvas
        # set UI.height htmlCanvasSize
        # set UI.width  htmlCanvasSize
        # set style [("border", "solid black 1px"), ("background", "#eee")]
        # set UI.id_ "canvas"

    clear <- UI.button #+ [string "Clear the canvas."]
    loadImg <- UI.button # set UI.text "Load Image"

    -- url <- UI.loadFile "image/jpg" "static/image.jpg"
    img <- UI.img # set UI.src "static/image.jpg"
           # set UI.id_ "img"
           
    let drawImage :: UI ()
        drawImage = do
          canvas # UI.drawImage img (0,0)


    -- selecting <- liftIO $ newIORef False
    -- -- startPoint :: IORef (Double,Double)
    -- startPoint <- liftIO $ newIORef (0 :: Double,0 :: Double)

    let eStartSelect = UI.mousedown canvas
    bStartSelect <- stepper (0,0) eStartSelect
    let bSetStart = (\s -> \p -> (s,p)) <$> bStartSelect
        eStopSelect = unionWith (\a b -> b) (const () <$> UI.mouseup canvas) (UI.leave canvas)
        eProcess = UI.mouseup canvas
        eMove = UI.mousemove canvas
    bSelecting <- stepper False $ unionWith (\a b -> b)
                     (const True <$> eStartSelect)
                     (const False <$> eStopSelect)
    bSelection <- stepper ((0,0),(0,0)) $ apply bSetStart eMove
    let eChangeSelection :: Event (Int,Int)
        eChangeSelection = whenE bSelecting eMove

    onEvent eStopSelect $ const $ do
      canvas # UI.clearCanvas
      drawImage
      
    onEvent eChangeSelection $ const $ do
      ((sx,sy),(x,y)) <- currentValue bSelection
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

    onEvent eProcess $ const $ do
      liftIO $ putStrLn "Processing Image"
      corners <- currentValue bSelection
      let (ul,lr) = orientate corners
      liftIO $ putStrLn (show (ul, lr))
      i <- liftIO $ I.readImageRGBA I.VU "static/image.jpg"
      let i' = recursiveImage ul lr 3 i
      liftIO $ I.writeImage "static/image.recursive.jpg" i'
      img # set' UI.src "static/image.recursive.jpg"
      return ()
      
    -- on UI.click clear $ const $
    --     canvas # UI.clearCanvas

    on UI.click loadImg $ const $ do
      let getWidth :: JSFunction Int
          getWidth = ffi "$('#img').width();"
          getHeight :: JSFunction Int
          getHeight = ffi "$('#img').height();"
      w <- callFunction getWidth
      h <- callFunction getHeight
      element canvas # set UI.width w
        # set UI.height h
      return ()


    void $ getBody window #+
      [ column [element canvas]
      , element img
      , element clear
      , element loadImg
      ]
    

type Point a = (a,a)
-- |Orientate corners so that the smallest values are the upper left corner and the biggest the lower right corner
orientate :: Point (Point Int) -> Point (Point Int)
orientate ((x1,y1),(x2,y2)) =
  ( ( min x1 x2
    , min y1 y2
    )
  , ( max x1 x2
    , max y1 y2
    )
  )
        
