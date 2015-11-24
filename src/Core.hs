module Core where
import FRP.Yampa
import Animal
import Types
import System.Random
import qualified Data.Map as Map

{-# LANGUAGE Arrows, BangPatterns #-}

process ::  StdGen -> SF GameInput GameOutput
process g = 
  let fs = randFishes 7 g
      col = Map.fromList $ zip [0..] fs in
  proc inp -> do
  rec 
   oOuts <- gameCore col -< (inp,oOuts)
  returnA -< Just (Map.elems $ Map.map obsState oOuts,quitEv inp)

gameCore :: Col Object -> SF (GameInput, Col ObjOutput) (Col ObjOutput) 
gameCore objs = 
    dpSwitch route
             objs
             (arr $ \a -> NoEvent)
             (\sfs f -> gameCore (f sfs))


route :: (GameInput, Col ObjOutput) -> Col sf -> Col (ObjInput, sf)
route (gi, oOuts) sfs =
  Map.mapWithKey route' sfs
   where route' k sf = (ObjInput gi (neigh k), sf)
         allNeighbours = neighbours oOuts
         neigh key =
          case Map.lookup key allNeighbours of
                Nothing -> []
                Just n ->
                  Map.elems $ Map.delete key (Map.map obsState n)



neighbours :: Col ObjOutput -> Col (Col ObjOutput)
neighbours m = Map.map neighbours' m
 where
 neighbours' v = 
  let p = pos . obsState $ v
      f = \objOut -> 300 >= (norm $ (pos . obsState $ objOut) ^-^ p) 
  in  Map.filter f m