{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import           Data.IORef
import           Data.Maybe                  (fromMaybe)
import           Numeric.Extra               (intToDouble)
import           Text.Read                   (readMaybe)

import qualified Codec.Picture               as C
import qualified Graphics.Image              as I

import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.UTF8        as UTF
import           System.Directory            (getCurrentDirectory)

import           Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    startGUI (defaultConfig { jsStatic = Just "static/", jsCustomHTML = Just "index.html", jsCallBufferMode = NoBuffering, jsAddr = Nothing, jsPort = Nothing }) setup

htmlCanvasSize = 400

setup :: Window -> UI ()
setup window = do
    return window # set title "Canvas - Examples"

    canvas <- UI.canvas
        # set UI.height htmlCanvasSize
        # set UI.width  htmlCanvasSize
        # set style [("border", "solid black 1px"), ("background", "#eee")]
        # set UI.id_ "canvas"

    canvas # UI.setLineDash [10, 10]

    clear <- UI.button #+ [string "Clear the canvas."]
    draw <- UI.button # set UI.text "Draw"
    loadImg <- UI.button # set UI.text "Load Image"
    file <- UI.input
            # set UI.type_ "file"
            # set UI.text "input filename"
            # set UI.id_ "file"

    img <- UI.img # set UI.id_ "img" # set UI.display "none"

    iptRecursiveSteps <- UI.input
    bRecursiveSteps <- stepper 3 $ validateRecursiveSteps <$> (readMaybe :: String -> Maybe Int) <$> UI.valueChange iptRecursiveSteps


    initialImage <- liftIO $ BS.readFile "static/image.jpg"
    (evtImage, hdlImage) <- liftIO $ (newEvent :: IO (Event BS.ByteString, Handler BS.ByteString))
    imgBytes <- stepper initialImage evtImage
    onChanges imgBytes (\bs -> do
      let encoded = UTF.toString $ B64.encode $ BS.toStrict bs
          sendEncoded :: JSFunction ()
          sendEncoded = ffi $ "fileString = '" ++ encoded ++ "';"
      liftIO $ putStrLn $ "Sending encoded image "
      runFunction sendEncoded
      )
    liftIO $ hdlImage initialImage

    let updateImage :: UI ()
        updateImage = do
          let updateImageFun :: JSFunction ()
              updateImageFun = ffi "img.src = 'data:image/jpg;base64,' + fileString; canvas.width = img.width; canvas.height = img.height;"
          runFunction updateImageFun

    let drawImage :: UI ()
        drawImage = do
          canvas # UI.drawImage img (0,0)
          return ()

    let eStartSelect = UI.mousedown canvas
    bStartSelect <- stepper (0,0) eStartSelect
    let bSetStart = (,) <$> bStartSelect
        eStopSelect = unionWith const (const () <$> UI.mouseup canvas) (UI.leave canvas)
        eProcess = UI.mouseup canvas
        eMove = UI.mousemove canvas
    bSelecting <- stepper False $ unionWith const
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
      -- todo get i from imgBytes instead of load from disk
      i <- liftIO $ I.readImageRGBA I.VS "static/image.jpg"
      rs <- currentValue bRecursiveSteps
      let i' = recursiveImage ul lr rs i
      liftIO $ I.writeImage "static/image.recursive.jpg" i'

      let iBytes = I.encode I.JPG [I.JPGQuality 255] i'
      liftIO $ hdlImage iBytes
      updateImage
      drawImage
      return ()

    on UI.click draw $ const $ do
      updateImage
      drawImage

    on UI.click loadImg $ const $ do
      let saveImageData :: JSFunction String
          saveImageData = ffi "fileString"
      b64 <- callFunction saveImageData
      flushCallBuffer

      let decoded = either (const initialImage) BS.fromStrict $ B64.decode $ UTF.fromString b64
      liftIO $ BS.writeFile "static/image.jpg" decoded
      liftIO $ print "Got image data"
      return ()

    void $ getBody window #+
      [ column [element canvas]
      , element img
      , element draw
      , element clear
      , element file
      , element loadImg
      , element iptRecursiveSteps
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

validateRecursiveSteps :: Maybe Int -> Int
validateRecursiveSteps (Just n)
  | n >= 0 = n
validateRecursiveSteps _ = 3
