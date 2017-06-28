module Data.Plagiarism.Utils
  (
    breakByEmptyLine
  , breakToSentences
  ) where

import           Data.Attoparsec.Text (anyChar, endOfLine, inClass, many',
                                       many1, manyTill, notInClass, parseOnly,
                                       satisfy, space)
import           Data.Text            (Text)
import qualified Data.Text            as T

breakByEmptyLine :: Text -> [Text]
breakByEmptyLine = either error (T.pack <$>) . parseOnly p .
                   (`T.append` T.pack "\n\n")
  where p = many' $ manyTill anyChar (endOfLine *> many1 endOfLine)

breakToSentences :: Text -> [Text]
breakToSentences = either error (T.pack <$>) . parseOnly p
  where p = many' $ (++) <$> many1 sentBody <*> many' sentEnd <* many' space
        sentBody = satisfy (notInClass ".?!")
        sentEnd  = satisfy (inClass ".?!")
