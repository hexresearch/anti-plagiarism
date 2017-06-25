module Main
  (
    main
  ) where

import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           System.Directory      (listDirectory)
import           System.FilePath.Posix ((</>))

import           Plagiarisms           (plagiarisms)
import           Types                 (Options (..), TextOrder (..))
import           Utils                 (breakByEmptyLine, breakToSentences)

main :: IO ()
main = do
  let dir = "data/referats"
  texts <- listDirectory dir >>= mapM (T.readFile . (dir </>))
  putStrLn . T.unpack $ plagiarisms opt texts
  where opt = Options
              { breakToPar   = breakByEmptyLine
              , breakToElem  = breakToSentences
              , textOrder    = Original
              , maxNumOfElem = 20
              , maxNumOfPar  = 5
              }
