module IO where 
import System.Random
import Types
import SDL
import Data.IORef
import StopWatch
import FRP.Yampa
import Sdl2
import Foreign.C.Types
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Show.Pretty (ppShow)
import qualified Linear as L



initialize :: IO GameInput
initialize = return $ GameInput 0 False

sense :: (IORef Stopwatch) -> Bool -> IO (DTime, Maybe GameInput)
sense ref _ = do
  (dt, timePassed) <- diffTime ref
  g <- getStdGen
    
  event      <- pollEvent            
        
  return (dt, Just (GameInput timePassed (quit event)))

 where 
  quit e =
   case e of
    Nothing -> False
    Just e' -> 
      case eventPayload e' of 
        QuitEvent -> True
        _                   -> False 

actuate :: Rendering -> Bool -> GameOutput -> IO Bool
actuate (Rendering rd ps bs fs) _ o = 
  case o of 
    Nothing -> return True
    Just (vs,q) ->
      do clear rd
         mapM_ (\s -> renderTexture rd s (0,0)) bs
         mapM_ renderObject vs 
         mapM_ (\s -> renderTexture rd s (0,0)) fs
         present rd
         return q
         
  where renderObject v = do
         let (x,y,_) = pos v
             (u,k,_) = vel v
             (a,b,_) = acc v

             (pn, pntSrc, w, h) = sprites v
             x' = round (x - (fromIntegral w)/2)
             y' = fromIntegral $ windowHeight
                  - round (y + (fromIntegral h)/2)

             x'' = round x
             y'' = fromIntegral $ windowHeight
                  - round y

             u' = round (x + u)
             k' = fromIntegral $ windowHeight
                  - round (y + k)

             a' = round (x + a)
             b' = fromIntegral $ windowHeight
                  - round (y + b )
         --putStrLn . ppShow $ (magic.pos $ v , magic.vel $ v, magic.acc $ v)
         --putStrLn . ppShow $ (x',y',u',k')
         


         renderSprite rd
                      (fromJust $ Map.lookup pn ps)
                      pntSrc
                      (x', y')
                      w h
         --dLine rd (255,0,0,255) (x'', y'') (a',b')
         --dLine rd (0,255,0,255) (x'', y'') (u',k')
         rendererDrawColor rd $= L.V4 0 0 0 0

magic :: Vec3 -> (Int,Int,Int)
magic (x,y,z) = (round x, round y, round z)