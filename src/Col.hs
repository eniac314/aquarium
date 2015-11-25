module Col where 
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Control.DeepSeq
import Types
import Helper
import Data.List (foldl')

type ColKey = (Int,ID)

data Col a =
 Col { colNextKey :: Int
      , colAssocs  :: Map.Map ColKey a
      }

instance Functor (Col) where
  fmap f c  = c { colAssocs = Map.map f (colAssocs c)}

emptyCol :: Col a
emptyCol = Col 0 Map.empty

lookupCol :: ColKey -> Col a -> Maybe a
lookupCol k c = Map.lookup k (colAssocs c)

insertCol :: a -> ID -> Col a -> Col a
insertCol o id col =
    let k = colNextKey col
        m = colAssocs col
        newAssocs = Map.insert (k,id) o m
    in  Col (k+1) newAssocs

listToCol :: ID -> [a] -> Col a
listToCol id xs =
 foldl' (\m o -> insertCol o id m) emptyCol xs 

elemsCol :: Col a -> [a]
elemsCol = Map.elems . colAssocs

assocsCol :: Col a -> [(ColKey,a)]
assocsCol = Map.toList . colAssocs

deleteCol :: ColKey -> Col a -> Col a
deleteCol k c = c { colAssocs = Map.delete k (colAssocs c)}

mapCol :: ((ColKey,a)->b) -> Col a -> Col b
mapCol f c = c { colAssocs = Map.mapWithKey (curry f) (colAssocs c)}

filterCol :: (a -> Bool) -> Col a -> Col a
filterCol p c = c { colAssocs = Map.filter p (colAssocs c)}

unionCol :: Col a -> Col a -> Col a
unionCol c1 c2 = 
    Col (colNextKey c1 + colNextKey c2)
        (Map.union (colAssocs c1) (colAssocs c2))
