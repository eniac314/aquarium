module Helper where
import FRP.Yampa
import Types
import System.Random
import qualified Data.Vector as Vec
import SDL hiding (time)

osc :: Int -> Int -> Int -> [Int]
osc a b c = cycle $ [a,a+c..b] ++ [b,b-c..a+c]

slow :: Int -> [a] -> [a]
slow 0 xs = xs
slow n [] = []
slow n (x:xs) = go n x [] ++ (slow n xs) where go 0 _ xs = xs
                                               go n x xs = go (n-1) x (x:xs)

getPos :: ObjOutput -> Pos
getPos = pos . obsState

getVel :: ObjOutput -> Pos
getVel = vel . obsState

getAcc :: ObjOutput -> Pos
getAcc = acc . obsState

swap (a,b) = (b,a)

--whnfList xs = List.foldl' (flip seq) () xs `seq` xs

--printState :: ObjectState -> IO ()
--printState ost =
-- let p = pos ost
--     n = idSt ost
--     e = envi
-- in putStrLn $ Pr.ppShow (n,p)

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

--randAcc :: StdGen -> SF () Vec3
--randAcc g = 
--  let (g1,g2) = split g
--  in proc inp -> do
--     x <- noiseR (-maxAccX,maxAccX) g1 -< ()
--     y <- noiseR (-maxAccY,maxAccY) g2 -< ()
--     returnA -< (x,y,0)

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

t = (take 2000) $ embed (testHold) (1, repeat (0.1,Nothing))

processSdlEvent :: Maybe SDL.Event -> SDLEvent
processSdlEvent event =  
 case event of
  Nothing -> NoSDLEvent
  Just e  ->
   case eventPayload e of 
    QuitEvent -> Quit
    KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ kc _)) ->
     case kc of KeycodeD -> DebugOn
                KeycodeEscape -> Quit
                _        -> NoSDLEvent
    _ -> NoSDLEvent 