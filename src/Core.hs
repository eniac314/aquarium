module Core where
import FRP.Yampa
import Bluefish
import Vegetation
import Types
import Col
import Helper
import System.Random
import Control.DeepSeq

{-# LANGUAGE Arrows, BangPatterns #-}

process ::  StdGen -> SF GameInput GameOutput
process g = 
  let bsw = baseSeaweed g
      ssw = randSeaweeds 24 g
      fs = randFishes 15 g
      c1 = (listToCol Bluefish) fs
      c2 = (listToCol SmallSeaweed) ssw
      c3 = insertCol (fst $ randSeaweed g) SmallSeaweed (unionCol c1 c2) in
  proc inp -> do
  rec 
   oOuts <- gameCore c3 -< (inp,oOuts)
  returnA -< Just (elemsCol $ fmap obsState oOuts,sdlEvent inp)

gameCore :: Col Object -> SF (GameInput, Col ObjOutput) (Col ObjOutput) 
gameCore objs = 
    dpSwitch route
             objs
             (arr $ \a -> NoEvent)
             (\sfs f -> gameCore (f sfs))


route :: (GameInput, Col ObjOutput) -> Col sf -> Col (ObjInput, sf)
route (gi, oOuts) sfs =
  mapCol route' sfs
   where route' (k,sf) = (ObjInput gi (neigh k), sf)
         allNeighbours = neighbours oOuts
         neigh key =
          case lookupCol key allNeighbours of
                Nothing -> []
                Just n ->
                  elemsCol $ deleteCol key (fmap obsState n)



neighbours :: Col ObjOutput -> Col (Col ObjOutput)
neighbours m = fmap neighbours' m
 where
 neighbours' v = 
  case getId v of
    BigSeaweed   -> emptyCol
    SmallSeaweed -> emptyCol
    _            ->
     
     let p = pos . obsState $ v
         f = \objOut -> 300 >= (norm $ (pos . obsState $ objOut) ^-^ p) 
     in  filterCol f m