module IO where 
import System.Random
import Types
import SDL hiding (Mouse)
import Data.IORef
import StopWatch
import FRP.Yampa
import Sdl2
import Helper
import Foreign.C.Types
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Text.Show.Pretty (ppShow)
import Data.List (sortOn)
import qualified Linear as L
import Control.DeepSeq

{-# LANGUAGE Arrows, BangPatterns, DeriveGeneric, DeriveAnyClass #-}



initialize :: IO GameInput
initialize = return $ GameInput 0 NoSDLEvent

sense :: (IORef Stopwatch) -> Bool -> IO (DTime, Maybe GameInput)
sense ref _ = do
  (dt, timePassed) <- diffTime ref
  g <- getStdGen
    
  event <- pollEvent            
        
  return (dt, Just (GameInput timePassed (processSdlEvent event)))

actuate :: Rendering -> Bool -> GameOutput -> IO Bool
actuate (Rendering rd ps l0 l1 l2 l3 f1) _ o = 
  case o of 
    Nothing -> return True
    Just (vs,e) ->
      do let (d,q,mp) =
              case e of
                NoSDLEvent  -> (False,False,Nothing)
                Quit        -> (False,True,Nothing)
                DebugOn     -> (True,False,Nothing)
                Mouse (x,y) -> (False,False,Just (x,y))
                _           -> (False,False,Nothing)
             (o0,o1,o2,o3) = toLevels vs levels

         clear rd
         
         mapM_ (\(s,p) -> renderTexture rd s p) l0
         mapM_ (renderObject d) o0
         mapM_ (\(s,p) -> renderTexture rd s p) l1
         mapM_ (renderObject d) o1
         mapM_ (\(s,p) -> renderTexture rd s p) l2
         mapM_ (renderObject d) (sortOn idSt (reverse (sortOn (y3.pos) o2)))
         mapM_ (\(s,p) -> renderTexture rd s p) l3
         mapM_ (renderObject d) o3
         mapM_ (\(s,p) -> renderTexture rd s p) f1  
         
         present rd
         --putStrLn . show $ mp
         --if isJust mp
         --then putStrLn . show $ fromJust mp
         --else return ()
         return q
         
  where renderObject d v = do
         let (x,y,_) = pos v
             (u,k,_) = vel v
             (a,b,_) = acc v
             id      = idSt v

             (pn, pntSrc, w, h) =
              if u <= 0 && idSt v == Bluefish
              then fst $ sprites v
              else snd $ sprites v

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
         
         


         renderSprite rd
                      (fromJust $ Map.lookup pn ps)
                      pntSrc
                      (x', y')
                      w h
         if d
         then do --putStrLn . ppShow $ (magic.pos $ v , magic.vel $ v, magic.acc $ v)
                 --putStrLn . ppShow $ round.norm.acc $ v
                 dLine rd (255,0,0,255) (x'', y'') (a',b')
                 dLine rd (0,255,0,255) (x'', y'') (u',k')
         else return ()

         rendererDrawColor rd $= L.V4 0 0 0 0

magic :: Vec3 -> (Int,Int,Int)
magic (x,y,z) = (round x, round y, round z)