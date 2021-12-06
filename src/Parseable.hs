module Parseable (Parseable, parser) where

import Control.Applicative
import Parser (Parser)
import qualified Parser as P

class Parseable a where
  parser :: Parser a

instance Parseable Bool where
  parser = P.constP "true" True <|> P.constP "false" False

instance Parseable Int where
  parser = P.wsP P.int
