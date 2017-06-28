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

import           Data.Plagiarism
import           Data.Plagiarism.Utils (breakByEmptyLine, breakToSentences)

main :: IO ()
main = do
  let dir = "data/referats"
  texts <- listDirectory dir >>= mapM (T.readFile . (dir </>))

  gen <- getStdGen
  let opt    = Options
               { breakToPar   = breakByEmptyLine
               , breakToElem  = breakToSentences
               , textOrder    = RandomElem
               , maxNumOfElem = 20
               , maxNumOfPar  = 5
               , randElemGen  = gen
               }
      pieces = fst $ plagiarism opt texts
  putStr $ T.unpack pieces
