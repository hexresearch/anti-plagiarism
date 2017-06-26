module Main
  (
    main
  ) where

import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           System.Directory      (listDirectory)
import           System.FilePath.Posix ((</>))
import           System.Random         (getStdGen)

import           Plagiarisms           (plagiarisms)
import           Types                 (Options (..), TextOrder (..))
import           Utils                 (breakByEmptyLine, breakToSentences)

main :: IO ()
main = do
  let dir = "data/referats"
  texts <- listDirectory dir >>= mapM (T.readFile . (dir </>))

  gen <- getStdGen
  let opt = Options
            { breakToPar   = breakByEmptyLine
            , breakToElem  = breakToSentences
            , textOrder    = Original
            , maxNumOfElem = 20
            , maxNumOfPar  = 5
            , randElemGen  = gen
            }
  putStrLn . T.unpack $ plagiarisms opt texts
