module Parseable (Parseable, parser, parseFromFile) where

import Control.Applicative
import Data.Either (Either)
import Located (Located (L))
import Parser (ParseError, Parser)
import qualified Parser as P
import qualified System.IO as IO
import qualified System.IO.Error as IO

class Parseable a where
  parser :: Parser a

  parseFromFile :: String -> IO (Either ParseError a)
  parseFromFile fileName = do
    IO.catchIOError
      ( do
          str <- IO.readFile fileName
          return $ parsedItem (P.parse parser str)
      )
      ( \e ->
          return $ Left $ "Error: " ++ show e
      )
    where
      parsedItem (Left e) = Left e
      parsedItem (Right (L _ it)) = Right it

instance Parseable Bool where
  parser = P.constP "true" True <|> P.constP "false" False

instance Parseable Int where
  parser = P.wsP P.int
