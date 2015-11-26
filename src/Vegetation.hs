module Vegetation where

import FRP.Yampa
import Types
import Helper
import System.Random
import qualified Data.Vector as Vec
import Control.DeepSeq
import Data.List (foldl')


bigSeaweedAnim = [(BigSeaweedPic,(x,0),438,348)| x <- [0,438..2190]]
smallSeaweedAnim = [(SmallSeaweedPic,(x,0),128,102)| x <- [0,128..640]]

seaweed :: ObjectInit -> Object
seaweed initial = 
  proc inp -> do

  r  <- noiseR (1,5) (gen0 initial)        -< ()
  
  e  <- repeatedly 1 ()                     -< ()

  r' <- hold 5                               -< tag e r
  
  s  <- listToSignal (sprites0 initial) 1 1 -< r'

  let st = Thing { pos = pos0 initial
                 , vel = vel0 initial
                 , acc = acc0 initial
                 , sprites = s
                 , idSt = id0 initial
                 }
  
      out = ObjOutput { obsState = deepseq st st
                      , killReq  = NoEvent
                      , spawnReq = NoEvent
                      }
  returnA -< out

baseSeaweed :: StdGen -> Object
baseSeaweed g =
  let cs = zip bigSeaweedAnim bigSeaweedAnim
      sw = ThingInit (700,150,0) nullVec nullVec cs g BigSeaweed
  in seaweed sw

randSeaweed :: StdGen -> (Object,StdGen)
randSeaweed g = 
  let (x,g1) = randomR (25, 775) g
      (y,g2) = randomR (25,150) g1
      cs = zip smallSeaweedAnim smallSeaweedAnim
      sw = ThingInit (x,y,0) nullVec nullVec cs g2 SmallSeaweed
  in (seaweed sw,g2)

randSeaweeds :: Int -> StdGen -> [Object]
randSeaweeds 0 _ = []
randSeaweeds n g = let (f,g') = randSeaweed g
                   in f:randSeaweeds (n-1) g'