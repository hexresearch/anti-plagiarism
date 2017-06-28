module Data.Plagiarism.Gen
  (
    plagiarism
  ) where

import           Control.Monad
import           Control.Monad.Trans.State (evalState, state)
import           Data.Function             (on)
import           Data.List                 (group, groupBy, sort, sortBy)
import           Data.Ord                  (comparing)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           System.Random.Shuffle     (shuffle')

import           Data.Plagiarism.Types     (Options (..), TextOrder (..),
                                            TextStruct (..))

-- | Creates plagiarism data out of provided corpus of texts.
--
-- It's not intended to use dlazily on the whole corpus of texts.
-- Please consider making a random subset oftexts before applying this function.
plagiarism :: Options -> [Text] -> (Text, [Int])
plagiarism opt = (\xs -> (flattenText xs, getSourceIds xs)) . groupAndSort . getRandomElems opt .
                  structText opt
  where groupAndSort :: [TextStruct] -> [[TextStruct]]
        groupAndSort xs =
          case textOrder opt of
            Original   -> map sort . groupByPar (maxNumOfPar opt) $
                          sortBy (comparing paragraph) xs
            RandomElem -> groupByPar (maxNumOfPar opt) $
                          sortBy (comparing paragraph) xs
            RandomAll  -> groupByPar (maxNumOfPar opt) xs


        flattenText :: [[TextStruct]] -> Text
        flattenText = concatPar . map (concatElem . map content)
          where concatPar  = T.intercalate (T.pack "\n\n")
                concatElem = T.intercalate (T.pack " ")

        getSourceIds :: [[TextStruct]] -> [Int]
        getSourceIds = fmap head . group . sort . fmap source . join

structText :: Options -> [Text] -> [TextStruct]
structText opt = concatMap structPar . zip [0 ..] . concatMap (\(n, txt) -> fmap (n,) $ breakToPar opt txt) . zip [0 ..]
  where structPar (n, (m, par))  = zipWith (structElem m n) [0 ..]
                              (breakToElem opt par)
        structElem m n k text = TextStruct { source    = m
                                           , paragraph = n
                                           , element   = k
                                           , content   = text
                                           }

getRandomElems :: Options -> [TextStruct] -> [TextStruct]
getRandomElems opt xs = take (maxNumOfElem opt) $
                        shuffle' xs (length xs) (randElemGen opt)

groupByPar :: Int -> [TextStruct] -> [[TextStruct]]
groupByPar n = map concat . groups . groupBy ((==) `on` paragraph)
  where groups :: [[TextStruct]] -> [[[TextStruct]]]
        groups xs = flip evalState xs . mapM (state . splitAt) $
                    partitions (length xs) n

        partitions :: Int -> Int -> [Int]
        partitions m k | k >= m    = replicate m 1
                       | otherwise = map (floor . ratio) [k - 1, k - 2 .. 0]
          where ratio :: Int -> Double
                ratio i = ((/) `on` fromIntegral) (m + i) k
