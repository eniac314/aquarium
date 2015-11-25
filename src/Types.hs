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
                            , sdlEvent :: SDLEvent
                            }

type GameOutput = Maybe ([ObjectState],SDLEvent)

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
             , sprites0 :: ![(Clip,Clip)]
             , gen0 :: !StdGen
             , id0 :: !ID
             } 

 | ThingInit { pos0 :: !Pos
             , vel0 :: !Vel
             , acc0 :: !Acc
             , sprites0 :: ![(Clip,Clip)]
             , gen0 :: !StdGen
             , id0 :: !ID
             } 

data ObjectState = 
    Animal { pos :: !Pos
           , vel :: !Vel
           , acc :: !Acc
           , sprites :: !(Clip,Clip)
           , idSt :: !ID         
           } 

  | Thing { pos :: !Pos
          , vel :: !Vel
          , acc :: !Acc
          , sprites :: !(Clip,Clip)
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

nullVec :: Vec3
nullVec = (0,0,0)

type Key = Int

type Clip = (PicName, Pnt, CInt, CInt)

data Direction = Lefty | Righty | Up | Down deriving (Ord,Eq,Show)

data Change = XOut | YOut | Impact | Swap

data PicName = FishPic
             | CrabPic
             | StonePic
             | BubblePic
             | BigSeaweedPic
             | SmallSeaweedPic deriving (Ord,Eq,Show,Generic,NFData)

data ID = Bluefish
        | Stone
        | BigSeaweed
        | SmallSeaweed
        | Bubble deriving (Ord,Eq,Show,Generic,NFData)


data SDLEvent = Quit | Mouse (Double,Double) | NoSDLEvent | DebugOn 

            

