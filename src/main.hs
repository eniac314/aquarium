import FRP.Yampa
import SDL hiding (Event,initialize,delay)
import Sdl2 
import Types
import IO 
import StopWatch
import Bluefish
import Core
import Control.Monad
import Helper
import qualified Data.Text as Text
import Foreign.C.Types
import qualified Linear as L
import Linear.Affine 
import qualified Text.Show.Pretty as Pr
import System.Random
import qualified SDL.Image as SDLI
import qualified Data.Map.Strict as Map

{-# LANGUAGE Arrows, BangPatterns #-}


initWindow = 
    defaultWindow
     { windowInitialSize = L.V2 (windowWidth) (windowHeight)}

vsyncRendererConfig = 
  RendererConfig
   { rendererType = AcceleratedVSyncRenderer
   , rendererTargetTexture = True
   }

main = do 
  initializeAll

  HintRenderScaleQuality $= ScaleLinear
  renderQuality <- get HintRenderScaleQuality
  when (renderQuality /= ScaleLinear) $
   putStrLn "Warning: Linear texture filtering not enabled!"
    
  window <- createWindow (Text.pack "aquarium 0.1") initWindow

  rd <- createRenderer window (-1) vsyncRendererConfig

  rendererDrawColor rd $= L.V4 0 0 0 0
  
  handle <- storeStopwatch

  g <- getStdGen

  f <- loadTexture rd "./images/bluefish.png"
  --bsw <- loadTexture rd "./images/seaweed_test.png"
  ssw <- loadTexture rd "./images/smallSeaweed.png"
  b1 <- loadTexture rd "./images/background.png"
  b2 <- loadTexture rd "./images/Back.png"
  b3 <- loadTexture rd "./images/MidRocks.png"
  b4 <- loadTexture rd "./images/Midstones.png"
  b5 <- loadTexture rd "./images/FrontGreenCorals.png"
  f1 <- loadTexture rd "./images/FrontRedCorals.png"

  let render = Rendering rd
                         (Map.fromList [(FishPic,f)
                                       ,(SmallSeaweedPic,ssw)])
                        [b1,b2,b3,b4,b5] [f1]
  
  reactimate initialize (sense handle) (actuate render) (process g)

  destroyRenderer rd
  destroyWindow window
  quit

 -------------------------------------------------------------------------


 