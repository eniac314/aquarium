module SeaHorse where

import FRP.Yampa
import Types
import Helper
import System.Random
import qualified Data.Vector as Vec
import Control.DeepSeq
import Data.List (foldl')


leftAnim = [(SeaHorsePic,(x,0),60,96) | x <- [0,60..180]]
rightAnim = [(SeaHorsePic,(x,96),60,96) | x <- [0,60..180]]
seaHorseMinDist = 50
pSeaHorse cur objOut = 
  200 >= (norm $ (pos . obsState $ objOut) ^-^ (pos . obsState $ cur))
vMax = 20
aMax = 5

seaHorse :: ObjectInit -> Object
seaHorse initial = 
  proc inp -> do
  rec
   rx  <- noiseR (1100,1350) g  -< ()
   ry  <- noiseR (25,500) g  -< ()

   e1 <- repeatedly 3 ()   -< ()
   np <- hold (1150,350,0) -< tag e1 (rx,ry,0.0)

   a  <- iPre a0 <<< (arr move) -< (p,v,inp,np)
   v  <- integral >>^ (^+^ v0) -< a
   p  <- integral >>^ (^+^ p0) -< v
                          
   e2 <- repeatedly 0.75 () -< ()
   na <- hold (norm a0)     -< tag e2 (norm a)
   

   s   <- listToSignal spr 1 15 -< na
    
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


move :: (Vec3,Vec3, ObjInput,Vec3) -> Vec3
move (p,vm,(ObjInput gi env),p') =
  let n = fromIntegral . length $ env
      distObj = norm (p ^-^ p') 
      speed = norm vm
      
      --friction
      fr = safeNormalize $ negateVector vm
      friction = (aMax*speed/vMax)*^fr

     --Separation
      separate = 
       let subSet = filter (\o -> seaHorseMinDist >= norm (pos o ^-^ p)) env
           n = fromIntegral . length $ subSet 
           getVec o = norm (pos o ^-^ p) *^ safeNormalize (p ^-^ pos o) 
           dm = map (\o -> getVec o) subSet
           (dx,dy,dz) = foldl' (^+^) nullVec dm
       in  if n /= 0 then safeNormalize  $ (dx,dy,dz)^/n else nullVec
  
  in if n /= 0 && separate /= nullVec
     then friction ^+^ (1.5*vMax) *^ separate
     else friction ^+^ (1*vMax)*^goBack2 p' p
  
randSeaHorse :: StdGen -> (Object, StdGen)
randSeaHorse g = 
  let (x,g1) = randomR (1050,1350) g
      (y,g2) = randomR (50,500) g1
      (u,g3) = randomR (-10,10) g2
      (v,g4) = randomR (-20,20) g3
      (a,g5) = randomR (-2,2) g4
      (b,g6) = randomR (-5,5) g5
      cs | u > 0     = zip leftAnim rightAnim
         | otherwise = zip rightAnim leftAnim
      sh = AnimalInit (x,y,0) (u,v,0) (a,b,0) cs g6 SeaHorse
  in (seaHorse sh, g6)

randSeaHorses :: Int -> StdGen -> [Object]
randSeaHorses 0 _ = []
randSeaHorses n g = let (f,g') = randSeaHorse g
                    in f:randSeaHorses (n-1) g'