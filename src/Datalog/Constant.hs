module Datalog.Constant where

import Control.Applicative
import Parseable (Parseable (parser))
import Parser
import qualified Parser as P
import Pretty (Pretty (pretty))

data Constant
  = Symbol String
  | Boolean Bool
  | Integer Int -- Maybe add | SymbolId Int
  deriving (Eq, Show)

instance Parseable Constant where
  parser = symbolP <|> booleanP <|> integerP
    where
      booleanP = Boolean <$> parser
      integerP = Integer <$> parser
      symbolP = Symbol <$> P.wsP quotedString
      dQuote = P.satisfy (== '"')
      notDQuote = P.satisfy (/= '"')
      quotedString = P.between dQuote (many notDQuote) dQuote

instance Pretty Constant where
  pretty (Symbol s) = s
  pretty (Boolean b) = show b
  pretty (Integer i) = show i
