module Types where

import SDL hiding (Event)
import FRP.Yampa
import Foreign.C.Types
import System.Random
import Sdl2
import qualified Data.Map as Map
import qualified Text.Show.Pretty as Pr
import GHC.Generics (Generic)
import Control.DeepSeq

{-# LANGUAGE Arrows, BangPatterns, DeriveGeneric, DeriveAnyClass #-}

windowWidth = 1366 :: CInt
windowHeight = 768 :: CInt
center = (0.5 * fromIntegral windowWidth, 0.5 * fromIntegral windowHeight,0) :: Vec3

type Scalar = Double
type Vec3 = (Scalar,Scalar,Scalar)
type Pos = Vec3
type Vel = Vec3
type Acc = Vec3

x3 :: Vec3 -> Scalar
x3 (x,y,z) = x

y3 :: Vec3 -> Scalar
y3 (x,y,z) = y

--------------------------------------------------------------------------
{- Game Types -}

data GameInput  = GameInput { dt :: !DTime
                            , quitEv :: !Bool
                            }

type GameOutput = Maybe ([ObjectState],Bool)

data Rendering  = Rendering { render    :: Renderer
                            , spriteMap :: Pics
                            , bStills    :: [Texture]
                            , fStills    :: [Texture]
                            }

type Pics = Map.Map PicName Texture

--------------------------------------------------------------------------
{- Objects Types -}

data ObjectInit = 
  AnimalInit { pos0 :: !Pos
             , vel0 :: !Vel
             , acc0 :: !Acc
             , sprites0 :: !([Clip],[Clip])
             , gen0 :: !StdGen
             , id0 :: !ID
             , direct :: Direction
             } 

 | ThingInit { pos0 :: !Pos
             , vel0 :: !Vel
             , acc0 :: !Acc
             , sprites0 :: !([Clip],[Clip])
             , gen0 :: !StdGen
             , id0 :: !ID
             } 

data ObjectState = 
    Animal { pos :: !Pos
           , vel :: !Vel
           , acc :: !Acc
           , sprites :: !Clip
           , idSt :: !ID         
           } 

  | Thing { pos :: !Pos
          , vel :: !Vel
          , acc :: !Acc
          , sprites :: !Clip
          , idSt :: !ID
          } deriving (Generic,NFData)



type Object = SF ObjInput ObjOutput

data ObjInput = 
 ObjInput { gi :: !GameInput
          , envi :: ![ObjectState]
          } 

data ObjOutput = 
 ObjOutput { obsState :: !ObjectState
           , killReq  :: !(Event ())
           , spawnReq :: !(Event [Object])
           } 

--------------------------------------------------------------------------
{- Miscs -}

type Key = Int

type Col a = Map.Map Key a

type Clip = (PicName, Pnt, CInt, CInt)

data Direction = Lefty | Righty | Up | Down deriving (Ord,Eq,Show)

data Change = XOut | YOut | Impact | Swap

data PicName = FishPic
             | CrabPic
             | StonePic
             | BubblePic deriving (Ord,Eq,Show,Generic,NFData)

data ID = BlueFish deriving (Ord,Eq,Show,Generic,NFData)

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
