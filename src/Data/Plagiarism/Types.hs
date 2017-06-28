module Data.Plagiarism.Types
  (
    Options (..)
  , TextOrder (..)
  , TextStruct (..)
  ) where

import           Data.Function (on)
import           Data.Monoid   ((<>))
import           Data.Text     (Text)
import           System.Random (StdGen)

data Options = Options
               { breakToPar   :: Text -> [Text]       -- ^ Breaks to paragraphs (chunks)
               , breakToElem  :: Text -> [Text]       -- ^ Breaks paragraphs to elements (chunks/stems)
               , textOrder    :: TextOrder
               , maxNumOfElem :: {-# UNPACK #-} !Int
               , maxNumOfPar  :: {-# UNPACK #-} !Int
               , randElemGen  :: {-# UNPACK #-} !StdGen
               }

data TextOrder = Original
               | RandomElem
               | RandomAll

data TextStruct = TextStruct
                  { source    :: {-# UNPACK #-} !Int
                  , paragraph :: {-# UNPACK #-} !Int
                  , element   :: {-# UNPACK #-} !Int
                  , content   :: {-# UNPACK #-} !Text
                  }

instance Eq TextStruct where
  x == y = ((==) `on` paragraph) x y && ((==) `on` element) x y
instance Ord TextStruct where
  compare x y = (compare `on` paragraph) x y <> (compare `on` element) x y
