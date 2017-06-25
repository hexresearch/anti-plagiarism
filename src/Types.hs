module Types
  (
    Options (..)
  , TextOrder (..)
  , TextStruct (..)
  , randomGen
  ) where

import           Data.Function (on)
import           Data.Monoid   ((<>))
import           Data.Text     (Text)
import           System.Random (StdGen, mkStdGen)

data Options = Options
               { breakToPar   :: Text -> [Text]
               , breakToElem  :: Text -> [Text]
               , textOrder    :: TextOrder
               , maxNumOfElem :: {-# UNPACK #-} !Int
               , maxNumOfPar  :: {-# UNPACK #-} !Int
               }

data TextOrder = Original
               | RandomElem
               | RandomAll

data TextStruct = TextStruct
                  { paragraph :: {-# UNPACK #-} !Int
                  , element   :: {-# UNPACK #-} !Int
                  , content   :: {-# UNPACK #-} !Text
                  }
instance Eq TextStruct where
  x == y = ((==) `on` paragraph) x y && ((==) `on` element) x y
instance Ord TextStruct where
  compare x y = (compare `on` paragraph) x y <> (compare `on` element) x y

randomGen :: StdGen
randomGen = mkStdGen 123
