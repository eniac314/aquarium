module Bluefish where

import FRP.Yampa
import Types
import Helper
import System.Random
import qualified Data.Vector as Vec
import Control.DeepSeq
import Data.List (foldl')

fishLeftAnim = [(FishPic,(x,0),99,37) | x <- [0,99..297]]
fishRightAnim = [(FishPic,(x,37),99,37) | x <- [0,99..297]]
fishMinDist = 130
maxAccX = 10
maxAccY = 5
vMax = 100
aMax = 20

fish :: ObjectInit -> Object
fish initial = 
  proc inp -> do
  rec
   rx  <- noiseR (-500.0,fromIntegral windowWidth+500) g  -< ()
   ry  <- noiseR (-500.0,fromIntegral windowHeight+500) g  -< ()

   e  <- repeatedly 5 () -< ()
   np <- hold center -< tag e (rx,ry,0.0)

   a  <- iPre a0 <<< (arr flocking) -< (p,v,inp,np)
   v  <- integral >>^ (^+^ v0) -< a
   p  <- integral >>^ (^+^ p0) -< v
                          
   
   s   <- listToSignal spr 1 15 -<   (norm a)
    
  let st  = Animal { pos = p
                   , vel = v
                   , acc = a
                   , sprites = s
                   , idSt = id0 initial
                   }
      out = ObjOutput { obsState = deepseq st st
                       , killReq  = NoEvent
                       , spawnReq = NoEvent
                       }
  returnA -< out 
              
   where 
    v0 = vel0 initial
    p0 = pos0 initial
    a0 = acc0 initial
    spr = sprites0 initial
    g = gen0 initial


flocking :: (Vec3,Vec3, ObjInput,Vec3) -> Vec3
flocking (p,vm,(ObjInput gi env),p') =
 
 let n  = fromIntegral . length $ env'
     distObj = norm (p ^-^ p') 
     speed = norm vm
     env' = filter (\o -> idSt o == Bluefish) env
     
     --friction
     fr = safeNormalize $ negateVector vm
     friction = (aMax*speed/vMax)*^fr
     
     --Cohesion
     avrgLoc =
      let (x,y,z) = foldl' (\a o -> pos o ^+^ a) (0,0,0) env'    
          (u,v,w) = ((x,y,z)^/n) ^-^ p
      in  safeNormalize $ (u/maxAccX,v/maxAccY,w) 
     --Alignment
     avrgHeading =
      let (vx,vy,vz) = foldl' (\a o -> vel o ^+^ a) (0,0,0) env'    
      in safeNormalize $ (vx,vy,vz)^/n
     --Separation
     separate = 
      let subSet = filter (\o -> fishMinDist >= norm (pos o ^-^ p)) env
          n = fromIntegral . length $ subSet 
          getVec o = norm (pos o ^-^ p) *^ safeNormalize (p ^-^ pos o) 
          dm = map (\o -> getVec o) subSet
          (dx,dy,dz) = foldl' (^+^) (0,0,0) dm
      in if n /= 0 then safeNormalize  $ (dx,dy,dz)^/n else nullVec

 in if n /= 0 
    then
     if separate == (0,0,0)
     then friction ^+^ (0.2*vMax)*^goBack2 p' p ^+^
          (0.2*vMax)*^((avrgLoc ^+^ avrgHeading) ^/ 2)
     else (1.5*vMax)*^separate
         
    else friction ^+^ (1*vMax)*^goBack2 p' p
 


 
randFish :: StdGen -> (Object,StdGen)
randFish g =
  let (x,g1) = randomR (25,fromIntegral windowWidth - 25) g
      (y,g2) = randomR (25,fromIntegral windowHeight - 25) g1
      (u,g3) = randomR (-100,100) g2
      (v,g4) = randomR (-40,40) g3
      (a,g5) = randomR (-10,10) g4
      (b,g6) = randomR (-5,5) g5
      cs = zip fishLeftAnim fishRightAnim
      fi = AnimalInit (x,y,0) (u,v,0) (a,b,0) cs g6 Bluefish
  in (fish fi,g6)

randFishes :: Int -> StdGen -> [Object]
randFishes 0 _ = []
randFishes n g = let (f,g') = randFish g
                 in f:randFishes (n-1) g'


