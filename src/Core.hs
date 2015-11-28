module Core where
import FRP.Yampa
import Bluefish
import SeaHorse
import Vegetation
import ShellBubbles
import Types
import Col
import Helper
import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import System.Random
import Control.DeepSeq


{-# LANGUAGE Arrows, BangPatterns #-}

routing :: Map.Map ID ([ID],(ObjOutput -> ObjOutput -> Bool))
routing = 
  Map.fromList [(Bluefish,([Bluefish,SmallSeaweed],pBluefish))
               ,(SmallSeaweed,([],pSeaWeed))
               ,(BigSeaweed,([],pSeaWeed))
               ,(SeaHorse,([SeaHorse],pSeaHorse))
               ,(Bubble,([],pBubble))
               ,(Shell,([],pShell))]

process ::  StdGen -> SF GameInput GameOutput
process g = 
  let ssw = randSeaweeds 7 g
      shs = randSeaHorses 3 g
      fs = randFishes 15 g
      bub = randBubbles 4 g
      she = baseShell g
      c1 = (listToCol Bluefish) fs
      c2 = (listToCol SmallSeaweed) ssw
      c3 = (listToCol SeaHorse) shs
      c4 = insertCol she Shell emptyCol
      c5 = foldl' unionCol emptyCol [c1,c2,c3,c4] in
  proc inp -> do
  rec 
   oOuts <- gameCore c5 -< (inp,oOuts)
  returnA -< Just (elemsCol $ fmap obsState oOuts,sdlEvent inp)

gameCore :: Col Object -> SF (GameInput, Col ObjOutput) (Col ObjOutput) 
gameCore objs = 
    dpSwitch route
             objs
             (arr killOrSpawn >>> notYet)
             (\sfs f -> gameCore (f sfs))


killOrSpawn :: (a, Col ObjOutput) -> Event (Col Object -> Col Object)
killOrSpawn (_, oOuts) = 
  foldl' (mergeBy (.)) NoEvent es
   where assEs = assocsCol oOuts
         es = [mergeBy (.) (tag (killReq oo) (deleteCol k)) (spawnReqs oo)
               | (k,oo) <- assEs]
         spawnReqs oo =
          case  (spawnReq oo) of
           NoEvent -> NoEvent
           Event os -> let fs = map (\(id,o) -> insertCol o id) os 
                       in Event (foldl' (.) id fs)

route :: (GameInput, Col ObjOutput) -> Col sf -> Col (ObjInput, sf)
route (gi, oOuts) sfs =
  mapCol route' sfs
   where route' (k,sf) = (ObjInput gi (neigh k), sf)
         
         neigh key =
          case lookupCol key allNeighbours of
                Nothing -> []
                Just n ->
                  elemsCol $ deleteCol key (fmap obsState n)
         
         allNeighbours = neighbours oOuts


neighbours :: Col ObjOutput -> Col (Col ObjOutput)
neighbours m = fmap neighbours' m
 where
 neighbours' v = 
  let id = getId v
      (speciesInput, pred) = fromJust $ Map.lookup id routing
      filterSpecies objOut = elem (getId objOut) speciesInput
  
  in filterCol (\o -> pred v o && filterSpecies o) m
  