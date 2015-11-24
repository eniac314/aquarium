module Animal where

import FRP.Yampa
import Types
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

basicFish :: ObjectInit -> Object
basicFish initial = update 
 where
  update =
   proc inp -> do
   rec
    rx  <- noiseR (-500.0,fromIntegral windowWidth+500) g  -< ()
    ry  <- noiseR (-500.0,fromIntegral windowHeight+500) g  -< ()

    e  <- repeatedly 5 () -< ()
    np <- hold center -< tag e (rx,ry,0.0)

    a  <- iPre a0 <<< (arr flocking) -< (p,v,inp,np)
    v  <- integral >>^ (^+^ v0) -< a
    p  <- integral >>^ (^+^ p0) -< v
                          
   
   s   <- listToSignal spr 4    -< log (norm a + 1)
    
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
    spr = fst . sprites0 $ initial
    g = gen0 initial

fish :: ObjectInit -> Object
fish initial = dSwitch update onChange
  where 
   update =  
    proc inp -> do
    objOut <- basicFish initial  -< inp
    let v = x3.getVel $ objOut 
    e3     <- edge -< (v < 0 && d == Righty 
                      || v >= 0 && d == Lefty)
    
    let t3 = tag e3 (Swap,objOut)

    returnA -< (objOut, t3)

   d = direct initial
   
   onChange (Swap,o) =
    let newFish =
         initial { pos0 = getPos o
                 , vel0 = getVel o
                 , acc0 = getAcc o
                 , sprites0 = swap . sprites0 $ initial
                 , direct = if d == Lefty
                            then Righty
                            else Lefty                          
                 }
    in fish newFish

clampVec vMax v
 | norm v > vMax = vMax *^ safeNormalize v
 | otherwise = v 

damp :: Time -> (Vec3 -> Vec3) -> SF Vec3 Vec3
damp fr f = 
  proc inp -> do
  e  <- repeatedly (1/fr) () -< ()
  v' <- accumHoldBy (\b v' -> f v') (1,1,1) -< tag e inp 
  returnA -< v'

damp2 :: SF Vec3 Vec3
damp2 =
 proc inp -> do
 t <- time -< ()
 returnA -< (inp)

randAcc :: StdGen -> SF () Vec3
randAcc g = 
  let (g1,g2) = split g
  in proc inp -> do
     x <- noiseR (-maxAccX,maxAccX) g1 -< ()
     y <- noiseR (-maxAccY,maxAccY) g2 -< ()
     returnA -< (x,y,0)


flocking :: (Vec3,Vec3, ObjInput,Vec3) -> Vec3
flocking (p,vm,(ObjInput gi env),p') =
 let n  = fromIntegral . length $ env
     distObj = norm (p ^-^ p') 
     speed = norm vm
     
     --friction
     fr = safeNormalize $ negateVector vm
     friction = (aMax*speed/vMax)*^fr
     
     --Cohesion
     avrgLoc =
      let (x,y,z) = foldl' (\a o -> pos o ^+^ a) (0,0,0) env    
          (u,v,w) = ((x/n),(y/n),z/n) ^-^ p
      in  safeNormalize $ (u/maxAccX,v/maxAccY,w) 
     --Alignment
     avrgHeading =
      let (vx,vy,vz) = foldl' (\a o -> vel o ^+^ a) (0,0,0) env    
      in safeNormalize $ (vx/n,vy/n,vz/n)
     --Separation
     separate = 
      let subSet = filter (\o -> fishMinDist >= norm (pos o ^-^ p)) env
          getVec o = norm (pos o ^-^ p) *^ safeNormalize (p ^-^ pos o) 
          dm = map (\o -> getVec o) subSet
          (dx,dy,dz) = foldl' (^+^) (0,0,0) dm
      in safeNormalize  (dx/n,dy/n,dz/n)

 in if n /= 0 
    then
     if separate == (0,0,0)
     then friction ^+^ (0.2*vMax)*^goBack2 p' p ^+^
          (0.2*vMax)*^((avrgLoc ^+^ avrgHeading) ^/ 2)
     else (1.5*vMax)*^separate
         --friction ^+^ (0.1*vMax)*^goBack2 p' p ^+^
         --(1*vMax)*^separate ^+^
         --(0.2*vMax)*^((avrgLoc ^+^ avrgHeading) ^/ 2)
         
    else friction ^+^ (1*vMax)*^goBack2 p' p
 
goBack :: Vec3 -> Vec3
goBack p = 
  let x = fromIntegral windowWidth / 2
      y = fromIntegral windowHeight / 2
      v = safeNormalize $ (x,y,0) ^-^ p 
  in v

goBack2 :: Vec3 -> Vec3 -> Vec3
goBack2 p' p = safeNormalize $ p' ^-^ p

safeNormalize :: Vec3 -> Vec3
safeNormalize v 
 | v == (0,0,0) = v
 | otherwise = normalize v

 
randFish :: StdGen -> (Object,StdGen)
randFish g =
  let (x,g1) = randomR (25,fromIntegral windowWidth - 25) g
      (y,g2) = randomR (25,fromIntegral windowHeight - 25) g1
      (u,g3) = randomR (-100,100) g2
      (v,g4) = randomR (-40,40) g3
      (a,g5) = randomR (-10,10) g4
      (b,g6) = randomR (-5,5) g5
      (cs,d) =
       if u >= 0 
       then ((fishRightAnim,fishLeftAnim),Righty)
       else ((fishLeftAnim,fishRightAnim),Lefty)
      fi = AnimalInit (x,y,0) (u,v,0) (a,b,0) cs g6 BlueFish d
  in (fish fi,g6)

randFishes :: Int -> StdGen -> [Object]
randFishes 0 _ = []
randFishes n g = let (f,g') = randFish g
                 in f:randFishes (n-1) g'


listToSignal :: [a] -> Int -> SF Scalar a
listToSignal xs i = 
 let v = Vec.fromList (slow i xs)
     n = Vec.length v
 in proc inp -> do
    t <- localTime -< inp
    returnA -< (Vec.!) v (mod (round (abs t*inp)) n)


testHold = 
  proc inp -> do 
  n <- listToSignal [1,2,3,4,5] 1 -< 1
  e <- repeatedly 0.5 () -< ()
  h <- hold 0 -< tag e n
  returnA -< h

--a = (4,-2,0) :: Vec3
t = (take 2000) $ embed (testHold) (1, repeat (0.1,Nothing))