module Plagiarisms
  (
    plagiarisms
  ) where

import           Control.Monad.Trans.State (evalState, state)
import           Data.Function             (on)
import           Data.List                 (groupBy, sort, sortBy)
import           Data.Ord                  (comparing)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           System.Random.Shuffle     (shuffle')

import           Types                     (Options (..), TextOrder (..),
                                            TextStruct (..))

plagiarisms :: Options -> [Text] -> Text
plagiarisms opt = flattenText . groupAndSort . getRandomElems opt .
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

structText :: Options -> [Text] -> [TextStruct]
structText opt = concatMap structPar . zip [0 ..] . concatMap (breakToPar opt)
  where structPar (n, par)  = zipWith3 structElem (repeat n) [0 ..]
                              (breakToElem opt par)
        structElem n k text = TextStruct { paragraph = n
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
