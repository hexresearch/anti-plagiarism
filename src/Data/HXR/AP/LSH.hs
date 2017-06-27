{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-all #-}

module Data.HXR.AP.LSH where

import           Data.Bifunctor             (bimap)
import           Data.Bits
import           Data.Default               (Default (..), def)
import           Data.Foldable              (toList)
import           Data.Hashable
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Proxy
import           Data.Set                   (Set)
import           Data.Text                  (Text)
import           Data.Vector                (Vector, (!))
import           Data.Vector.Instances      ()
import           Data.Word

import qualified Data.Char                  as C
import qualified Data.List                  as L
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Vector                as V

import           Data.HXR.AP.LSH.StaticData (salts2048)

class ( Num (MinHashElem a)
     ,  Eq (TextRep a)
     ,  Ord (TextRep a)
     ,  Hashable (TextRep a)
     ) =>  LshIndex a where
  data TextRep a    :: *

  type MinHashElem a :: *
  type MinHashRep a  :: * -> *

  normalize      :: TextRep a -> TextRep a

  genShingle     :: TextRep a -> Maybe (TextRep a, TextRep a)

  shingles :: TextRep a -> [TextRep a]
  shingles txt = uniq $ L.unfoldr genShingle (normalize txt)
    where uniq = S.toList . S.fromList

  hashedShingles :: TextRep a -> [Int]
  hashedShingles = fmap ((`mod` (shingleHashMax (Proxy :: Proxy a))) . hash) . shingles

  salts          :: a -> [Int]
  lshSplitFactor :: a -> Int

  hashfuns       :: a -> [(Int, Int -> Int)]
  hashfuns x = fmap (\(i,n) -> (i,\y -> hhash n y)) (zip [0..] (salts x))
    where
      {-# INLINE hhash #-}
      hhash !n !y = (y `xor` n) `mod` hashMax x

  hashMax :: a -> Int

  shingleHashMax :: Proxy a -> Int
  shingleHashMax _ = maxBound `div` 2

  minHashRepFromList :: a -> [Int] -> MinHashRep a (MinHashElem a)

  splitMinHash :: a -> MinHashRep a (MinHashElem a) -> [MinHashRep a (MinHashElem a)]

kJaccard :: (Ord a, Foldable f) => f a -> f a -> Double
kJaccard a' b' = ilen / ulen
  where ilen = fromIntegral $ S.size (a `S.intersection` b)
        ulen = fromIntegral $ S.size (a `S.union` b)
        a = S.fromList (foldMap return a')
        b = S.fromList (foldMap return b')

kJaccardMinHash :: ( LshIndex a
                   , Foldable (MinHashRep a)
                   , Num (MinHashElem a)
                   , Eq (MinHashElem a)
                   , Ord (TextRep a)
                   )
                => a
                -> MinHashRep a (MinHashElem a)
                -> MinHashRep a (MinHashElem a)
                -> Double

kJaccardMinHash _ a b = fromIntegral eq / fromIntegral tot
  where (eq,tot) = foldl fld (0,0) (zip (toList a) (toList b))
        fld (eq,ttl) (a,b) | a == 0 && b == 0 = (eq, ttl)
                           | a == b = (eq+1, ttl+1)
                           | a /= b = (eq, ttl+1)



minHash :: (LshIndex a, Show (TextRep a))
        => a
        -> TextRep a
        -> MinHashRep a (MinHashElem a)

minHash idx txt = minHashRepFromList idx $ hashez2
  where
    fns = L.sortOn fst $ hashfuns idx

    hashed = hashedShingles txt

    hashez2 = map fqn fns
      where fqn (_,f) = foldl (\v h -> min v (f h)) (maxBound :: Int) hashed
            {-# INLINE fqn #-}

defCyrEngAlphabet :: Text
defCyrEngAlphabet = T.concat [ nums
                             , eng
                             , T.toUpper eng
                             , cyr
                             , T.toUpper cyr
                             ]
  where
    nums = "0123456789"
    eng  = "abcdefghijklmnopqrstuvwxyz"
    cyr  = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя"


data NormalizeTextOpts = NormalizeTextOpts
                         { normPunct     :: Maybe Text
                         , normToLower   :: Bool
                         , normAlphabet  :: Text
                         , normFiltWords :: Text -> Bool
                         }

instance Default NormalizeTextOpts where
  def = NormalizeTextOpts { normPunct = Nothing
                          , normToLower = True
                          , normAlphabet = "abcdefghijklmnopqrstuvwxyz0123456789"
                          , normFiltWords = const True
                          }

normalizeText :: NormalizeTextOpts
              -> Text
              -> Text

normalizeText opts source =
  T.unwords $ filter (normFiltWords opts)
            $ T.words
            $ repl
            $ lc
            $ source
  where
    alph = normAlphabet opts <> fromMaybe "" (normPunct opts) <> " "
    alphSet = T.foldl (\s c -> S.union s (S.singleton c)) S.empty alph
    repl = T.map (\c -> if S.member c alphSet then c else ' ')
    lc = if (normToLower opts) then T.toLower else id


makeChunks :: Text -> [Text]
makeChunks = filter (not . T.null) . rejoin . breakPara . norm1
  where
    o = def { normToLower = False, normPunct = Just ".", normAlphabet = defCyrEngAlphabet }

    block = 200

    breakPara s = map T.pack $ go ([],[]) (T.unpack s)
      where

--         go :: (!a,!b) x -> (a!,!b)

        go (tok,acc) [] = reverse (reverse tok : acc)

        go (tok,acc) ('.':' ':c:cs) | C.isUpper c = go ([], reverse tok : acc) (c:cs)
                                    | otherwise = go (c : ' ' : tok,acc) cs

        go (tok,acc) (c:cs) = go (c : tok,acc) cs


    rejoin txt = go txt
      where

        go [] = []

        go (p1:p2:ps) = if (T.length p1 + T.length p2) < block
                          then go (mconcat [p1, " ", p2] : ps)
                          else p1 : go (p2:ps)

        go (p:[]) = [p]


    norm1 = normalizeText o

genShingleText :: Int -> Text -> (Maybe (Text, Text))
genShingleText n txt = if T.length txt < n
                         then Nothing
                         else Just (T.take n txt, T.drop 1 txt)

data LshTextSimpleK9 = LshTextSimpleK9

instance LshIndex LshTextSimpleK9 where
  data TextRep LshTextSimpleK9 = LshTextK9 { unLshTextK9 :: !Text }
                                 deriving (Eq,Ord,Show)

  type MinHashElem LshTextSimpleK9 = Word8
  type MinHashRep LshTextSimpleK9 = Vector

  normalize (LshTextK9 txt)  = LshTextK9 $ normalizeText opts txt
    where
      opts = def { normAlphabet = defCyrEngAlphabet
                 , normFiltWords = (\x -> T.length x > 2)
                 }

  genShingle (LshTextK9 txt) = bimap LshTextK9 LshTextK9 <$> genShingleText 10 txt

  hashedShingles (LshTextK9 txt) = V.toList gen
    where
      gen = V.unfoldr mkHash wws

      mkHash x = if V.null x
                   then Nothing
                   else Just (hash (V.take 10 x), V.drop 1 x)

      wws :: Vector Int
      wws = V.fromList (map (fromIntegral . C.ord) (T.unpack txt))

  salts          = const (take 60 salts2048)
  lshSplitFactor = const 5

  minHashRepFromList _ xs = V.fromList (fmap fromIntegral xs)

  hashMax _ =  (2 ^ 14) -- fromIntegral $ ((maxBound :: MinHashElem LshTextSimpleK9))
--   hashMax _ =  fromIntegral ((maxBound :: MinHashElem LshTextSimpleK9))

  splitMinHash a k = V.toList (V.unfoldr unf k)
    where
      unf vec | V.null vec = Nothing
              | otherwise = pure (V.splitAt (lshSplitFactor a) vec)

instance Hashable (TextRep LshTextSimpleK9) where
  hashWithSalt n = hashWithSalt n . unLshTextK9

makeKeyBlock :: Ord b => Text
                      -> b
                      -> [(MinHashRep LshTextSimpleK9 (MinHashElem LshTextSimpleK9), Set b)]

makeKeyBlock s b = zip keys (repeat (S.singleton b))
  where
    !chunks = makeChunks s
    !mhs = fmap (minHash LshTextSimpleK9 . normalize . LshTextK9) chunks
    !keys = foldMap (splitMinHash LshTextSimpleK9) mhs


