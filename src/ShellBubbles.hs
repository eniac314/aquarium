module ShellBubbles where

import FRP.Yampa
import Types
import Helper
import System.Random
import qualified Data.Vector as Vec
import Control.DeepSeq
import Data.List (foldl')

smallBubble = [(BubblePic,(0,0),114,114)]
midBubble = [(BubblePic,(114,0),114,114)]
bigBubble = [(BubblePic,(228,0),114,114)]

shellClosed  = (ShellPic,(0,0),164,105)
shellOpening = (ShellPic,(164,0),164,105)
shellOpened  = (ShellPic,(328,0),164,105)

shellSpr = cycle $ [shellClosed,shellOpening,shellOpened]

bubble :: ObjectInit -> Object 
bubble initial = 
  proc inp -> do
  
  v  <- integral >>^ (^+^ v0) -< a0
  p  <- integral >>^ (^+^ p0) -< v

  t  <- time                  -< ()

  let st = Thing { pos = p
                 , vel = v
                 , acc = a0
                 , sprites = s0
                 , idSt = id0 initial
                 }
  
      out = ObjOutput { obsState = deepseq st st
                      , killReq  = if t > 15 
                                   then Event ()
                                   else NoEvent
                      , spawnReq = NoEvent
                      }
  returnA -< out

  where a0 = acc0 initial
        v0 = vel0 initial
        p0 = pos0 initial
        s0 = head . sprites0 $ initial

randBubble :: StdGen -> (Object,StdGen)
randBubble g = 
  let (x,g1) = randomR (740, 830) g
      y      = 138
      (u,v)  = (0,50)
      (n,g2) = randomR (0,30) g1
      cs | n+u < 10 = zip smallBubble smallBubble
         | n < 20 = zip midBubble midBubble
         | otherwise = zip smallBubble smallBubble
      b = ThingInit (x,y,0) (u,v,0) nullVec cs g2 Bubble
  in (bubble b, g2)

randBubbles :: Int -> StdGen -> [Object]
randBubbles 0 _ = []
randBubbles n g = let (f,g') = randBubble g
                  in f:randBubbles (n-1) g'



data ShellStatus = Closed | Opening | Opened deriving Eq

shell :: ShellStatus -> ObjectInit -> Object
shell ss initial = dSwitch shell' onClick
 where
  shell' = 
   proc (ObjInput (GameInput _ ev) _) -> do
   
   --e1 <- edge       -< isClosed
   --e2 <- edge       -< isOpening
   --e3 <- edge       -< isOpened
   
   ec <- edgeTag Opening -< isClosed && clicked ev

   e3 <- after 0.1 Opened  -< ()
   e4 <- after 0.5 Closed  -< ()

   --e5 <- after 1 Opened  -< isEvent e2
   --e6 <- after 2 Closed  -< isEvent e3

   let st = Thing { pos = pos0 initial
                  , vel = vel0 initial
                  , acc = acc0 initial
                  , sprites = head . sprites0 $ initial
                  , idSt = id0 initial
                  }
  
       out = ObjOutput { obsState = deepseq st st
                       , killReq  = NoEvent
                       , spawnReq = bubbleDispenser
                       }
       e = merge ec (merge (gate e3 isOpening)
                           (gate e4 isOpened))
       --e = ec

   returnA -< (out,e)
   
   where
    clicked ev =
        case ev of
         MouseL (x,y) ->
          if x > 740 && x < 830 &&
             y > 600   && y < 650 
          then True else False
         _            -> False
    
    isOpened  = ss == Opened
    isOpening = ss == Opening
    isClosed  = ss == Closed
    
    g = gen0 initial
    
    bubbleDispenser
     | isOpening = Event [(Bubble,(fst . randBubble $ g))]
     | otherwise = NoEvent

  onClick ev = shell ev (nextSpr initial)
   where nextSpr ini =
          ini { sprites0 = tail . sprites0 $ ini
              , gen0 = snd . next $ (gen0 initial)
              } 

baseShell :: StdGen -> Object
baseShell g = 
  let cs = zip shellSpr shellSpr
      sh = ThingInit (785,140,0) nullVec nullVec cs g Shell
  in shell Closed sh
