module Main
  (
    main
  ) where

import           Data.Function         (on)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           System.Directory      (listDirectory)
import           System.FilePath.Posix ((</>))
import           System.Random         (getStdGen)

import           Data.HXR.AP.LSH       (TextRep (..), hashedShingles, kJaccard)
import           Plagiarisms           (plagiarisms)
import           Types                 (Options (..), TextOrder (..))
import           Utils                 (breakByEmptyLine, breakToSentences)

main :: IO ()
main = do
  let dir = "data/referats"
  texts <- listDirectory dir >>= mapM (T.readFile . (dir </>))

  gen <- getStdGen
  let opt    = Options
               { breakToPar   = breakByEmptyLine
               , breakToElem  = breakToSentences
               , textOrder    = Original
               , maxNumOfElem = 20
               , maxNumOfPar  = 5
               , randElemGen  = gen
               }
      pieces = plagiarisms opt texts
  putStr "Jaccard: "
  print $ (kJaccard `on` hashedShingles . LshTextK9) (T.concat texts) pieces
